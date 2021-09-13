"""Higher-level tracking of the semantics of specific commands."""

from argo_client.connection import ServerConnection

from abc import abstractmethod
from typing import Any, Dict, Optional, Tuple
from typing_extensions import Protocol


class HasServerConnection(Protocol):
    server_connection: ServerConnection

class HasProtocolState(Protocol):
    def protocol_state(self) -> Any: ...
    server_connection: ServerConnection


class ArgoException(Exception):
    message: str
    data: Dict[str, Any]
    code: int
    stdout: str
    stderr: str
    """A Python representation of the underlying JSON RPC error."""

    def __init__(self,
                 code: int,
                 message: str,
                 data: Any,
                 stdout: str,
                 stderr: str) -> None:
        super().__init__(message)
        self.message = message
        self.data = data
        self.code = code
        self.stdout = stdout
        self.stderr = stderr


class Interaction:
    """A representation of a concrete stateful interaction (i.e., a command or query)
    with the server. Applications should subclass this according to their needs.

    Subclasses should call the superclass constructor with the method
    name and the parameters. They should additionally implement methods
    ``state()`` and ``result()`` that return the protocol state and
    the result from a non-error response to the method. Both methods
    may call ``self.raw_result()`` to get the JSON RPC response
    associated with the resquest.

    """
    _method: str
    _params: Dict[str, Any]

    def __init__(self, method: str, params: Dict[str, Any],
                 connection: HasProtocolState,
                 *,
                 timeout: Optional[float]) -> None:

        self.connection = connection
        self._raw_response = None
        self.init_state = connection.protocol_state()
        self._method = method
        self._params = params
        self.add_param('state', self.init_state)
        self.request_id = \
            connection. \
            server_connection. \
            send_command(self._method, self._params, timeout=timeout)

    def add_param(self, name: str, val: Any) -> None:
        self._params[name] = val

    def raw_result(self) -> Any:
        """Get the JSON response associated with the request. Blocks until the
        reply is received.
        """
        if self._raw_response is None:
            self._raw_response = \
                self.connection. \
                server_connection. \
                wait_for_reply_to(self.request_id)
        return self._raw_response

    @abstractmethod
    def state(self) -> Any:
        """Subclasses should implement this method to return the protocol
        state after the RPC call is complete. This should be obtained from
        the result of ``self.raw_result()``.
        """
        pass

    @abstractmethod
    def result(self) -> Any:
        """Subclasses should implement this method to return the protocol
        result after the RPC call is complete and succeeds. This
        should be obtained from the result of ``self.raw_result()``.
        """
        pass

    @abstractmethod
    def stdout(self) -> str:
        """Subclasses should implement this method to report the stdout yielded
        by the interaction. This should be obtained from the result of
        ``self.raw_result()``."""
        pass

    @abstractmethod
    def stderr(self) -> str:
        """Subclasses should implement this method to report the stderr yielded
        by the interaction. This should be obtained from the result of
        ``self.raw_result()``."""
        pass

    @abstractmethod
    def process_result(self, result: Any) -> Any:
        """Subclasses should override this, to transform a JSON-encoded result
        into the application-specific result.
        """
        pass

    def process_error(self, exception: ArgoException) -> Exception:
        """Subclasses may override this to specialize exceptions to their own
        domain. The default implementation returns the exception unchanged.
        """
        return exception


class Command(Interaction):
    """A higher-level interface to a JSON RPC command that follows Argo
    conventions.

    In particular, for a non-error result, the actual result should be
    in the `answer` field of the `result` dictionary, and the
    resulting state should be in the `state` field.

    Subclasses should implement ``process_result``, which transforms a
    dictionary representation of a JSON answer object into the
    corresponding command's appropriate representation.
    """

    def state(self) -> Any:
        """Return the protocol state after the command is complete if the
        command did not error, or the protocol state prior to the the command
        otherwise."""
        res = self.raw_result()
        if 'error' in res:
            return self.init_state
        elif 'result' in res:
            return res['result']['state']
        else:
            raise ValueError("Invalid result type from JSON RPC")

    def _result_and_state_and_out_err(self) -> Tuple[Any, Any, str, str]:
        res = self.raw_result()
        if 'error' in res:
            msg = res['error']['message']
            error_data = None
            if 'data' in res['error'] and 'data' in res['error']['data']:
                error_data = res['error']['data']['data']
                msg += " " + str(error_data)
            exception = ArgoException(res['error']['code'],
                                      msg,
                                      error_data,
                                      res['error']['data']['stdout'],
                                      res['error']['data']['stderr'])
            raise self.process_error(exception)
        elif 'result' in res:
            return (res['result']['answer'],
                    res['result']['state'],
                    res['result']['stdout'],
                    res['result']['stderr'])
        else:
            raise ValueError("Invalid result type from JSON RPC")

    def result(self) -> Any:
        """Return the result of the command."""
        return self.process_result(self._result_and_state_and_out_err()[0])

    def stdout(self) -> str:
        """Return the stdout printed during the execution of the command."""
        return self._result_and_state_and_out_err()[2]

    def stderr(self) -> str:
        """Return the stderr printed during the execution of the command."""
        return self._result_and_state_and_out_err()[3]


class Query(Interaction):
    """A higher-level interface to a JSON RPC query that follows Argo
    conventions.

    In particular, for a non-error result, the actual result should be
    in the ``answer`` field of the ``result`` dictionary. Because queries
    do not change the state, it will be returned unchanged.

    Subclasses should implement ``process_result``, which transforms a
    dictionary representation of a JSON answer object into the
    corresponding command's appropriate representation.

    """

    def state(self) -> Any:
        """Return the state prior to the query, because queries don't change
        the state.
        """
        return self.init_state

    def _result_and_out_err(self) -> Tuple[Any, str, str]:
        res = self.raw_result()
        if 'error' in res:
            msg = res['error']['message']
            if 'data' in res['error']:
                msg += " " + str(res['error']['data']['data'])
            exception = ArgoException(res['error']['code'],
                                      msg,
                                      res['error'].get('data').get('data'),
                                      res['error']['data']['stdout'],
                                      res['error']['data']['stderr'])
            raise self.process_error(exception)
        elif 'result' in res:
            return (res['result']['answer'],
                    res['result']['stdout'],
                    res['result']['stderr'])
        else:
            raise ValueError("Invalid result type from JSON RPC")

    def result(self) -> Any:
        """Return the result of the query."""
        return self.process_result(self._result_and_out_err()[0])

    def stdout(self) -> str:
        """Return the stdout printed during the execution of the command."""
        return self._result_and_out_err()[1]

    def stderr(self) -> str:
        """Return the stderr printed during the execution of the command."""
        return self._result_and_out_err()[2]


class Notification:
    """A representation of a concrete stateless interaction with the server.
    Applications should subclass this according to their needs.

    Subclasses should call the superclass constructor with the method
    name and the parameters. 

    """
    _method: str
    _params: Dict[str, Any]

    def __init__(self, method: str, params: Dict[str, Any],
                 connection: HasServerConnection) -> None:
        self.connection = connection
        self._method = method
        self._params = params
        connection.server_connection.send_notification(self._method, self._params)

