"""Higher-level tracking of the semantics of specific commands."""

from argo.connection import ServerConnection

from typing import Any, Dict, Tuple
from typing_extensions import Protocol

class HasProtocolState(Protocol):
    def protocol_state(self) -> Any: ...
    server_connection : ServerConnection

class Interaction:
    """A representation of a concrete interaction with the
    server. Applications should subclass this according to their needs.

    Subclasses should call the superclass constructor with the method
    name and the parameters. They should additionally implement methods
    ``state()`` and ``result()`` that return the protocol state and
    the result from a non-error response to the method. Both methods
    may call ``self.raw_result()`` to get the JSON RPC response
    associated with the resquest.

    """
    _method : str
    _params : Dict[str, Any]

    def __init__(self, method : str, params : Dict[str, Any], connection : HasProtocolState) -> None:

        self.connection = connection
        self._raw_response = None
        self.init_state = connection.protocol_state()
        self._method = method
        self._params = params
        self.add_param('state', self.init_state)
        self.request_id = \
            connection. \
            server_connection. \
            send_message(self._method, self._params)

    def add_param(self, name : str, val : Any) -> None:
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

    def state(self) -> Any:
        """Subclasses should implement this method to return the protocol
        state after the RPC call is complete. This should be obtained from
        the result of ``self.raw_result()``.
        """
        raise NotImplementedError('state')

    def result(self) -> Any:
        """Subclasses should implement this method to return the protocol
        result after the RPC call is complete and succeeds. This
        should be obtained from the result of ``self.raw_result()``.
        """
        raise NotImplementedError('result')


class ArgoException(Exception):
    """A Python representation of the underlying JSON RPC error."""
    def __init__(self, message : str, data : Any) -> None:
        super().__init__(message)
        self.data = data


class Command(Interaction):
    """A higher-level interface to a JSON RPC command that follows Argo conventions.

    In particular, for a non-error result, the actual result should be
    in the `answer` field of the `result` dictionary, and the
    resulting state should be in the `state` field.

    Subclasses should implement ``process_result``, which transforms a
    dictionary representation of a JSON answer object into the
    corresponding command's appropriate representation.
    """


    def _result_and_state(self) -> Tuple[Any, Any]:
        res = self.raw_result()
        if 'error' in res:
            msg = res['error']['message']
            if 'data' in res['error']:
                msg += " " + str(res['error']['data'])
            raise ArgoException(msg, res['error'].get('data'))
        elif 'result' in res:
            return (res['result']['answer'], res['result']['state'])
        else:
            raise ValueError("Invalid result type from JSON RPC")

    def process_result(self, result : Any) -> Any:
        """Subclasses should override this, to transform a JSON-encoded result
        into the application-specific result.
        """
        raise NotImplementedError('process_result')

    def state(self) -> Any:
        """Return the protocol state after the command is complete."""
        return self._result_and_state()[1]

    def result(self) -> Any:
        """Return the result of the command."""
        return self.process_result(self._result_and_state()[0])

class Query(Interaction):
    """A higher-level interface to a JSON RPC query that follows Argo conventions.

    In particular, for a non-error result, the actual result should be
    in the ``answer`` field of the ``result`` dictionary. Because queries
    do not change the state, it will be returned unchanged.

    Subclasses should implement ``process_result``, which transforms a
    dictionary representation of a JSON answer object into the
    corresponding command's appropriate representation.

    """

    def state(self) -> Any:
        """Return the state prior to the query, because queries don't change the
        state.
        """
        return self.init_state

    def _result(self) -> Any:
        res = self.raw_result()
        if 'error' in res:
            msg = res['error']['message']
            if 'data' in res['error']:
                msg += " " + str(res['error']['data'])
            raise ArgoException(msg, res['error'].get('data'))
        elif 'result' in res:
            return res['result']['answer']

    def process_result(self, result : Any) -> Any:
        """Subclasses should override this, to transform a JSON-encoded result
        into the application-specific result.
        """
        raise NotImplementedError('process_result')


    def result(self) -> Any:
        """Return the result of the query."""
        return self.process_result(self._result())
