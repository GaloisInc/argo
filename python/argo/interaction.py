class Interaction():
    """A representation of a concrete interaction with the
    server. Applications should subclass this according to their needs.

    Subclasses should set ``self.method`` to a string and
    ``self.params`` to a dictionary before calling the superclass
    constructor. They should additionally implement methods
    ``state()`` and ``result()`` that return the protocol state and
    the result from a non-error response to the method. Both methods
    may call ``self.raw_result()`` to get the JSON RPC response
    associated with the resquest.
    """

    def __init__(self, connection):
        if 'method' not in self.__dict__:
            raise NotImplementedError('self.method')
        if 'params' not in self.__dict__:
            raise NotImplementedError('self.params')

        self.connection = connection
        self._raw_response = None
        self.init_state = connection.protocol_state()
        self.params['state'] = self.init_state
        self.request_id = connection.server_connection.send_message(self.method, self.params)

    def raw_result(self):
        """Get the JSON response associated with the request. Blocks until the
        reply is received.
        """
        if self._raw_response is None:
            self._raw_response = self.connection.server_connection.wait_for_reply_to(self.request_id)
        return self._raw_response

    def state(self):
        """Subclasses should implement this method to return the protocol
        state after the RPC call is complete. This should be obtained from
        the result of ``self.raw_result()``.
        """
        raise NotImplementedError('state')

    def result(self):
        """Subclasses should implement this method to return the protocol
        result after the RPC call is complete and succeeds. This
        should be obtained from the result of ``self.raw_result()``.
        """
        raise NotImplementedError('result')

class ArgoException(Exception):
    pass

class Command(Interaction):
    """A higher-level interface to a JSON RPC command that follows Argo conventions.

    In particular, for a non-error result, the actual result should be
    in the `answer` field of the `result` dictionary, and the
    resulting state should be in the `state` field.

    Subclasses should implement ``process_result``, which transforms a
    dictionary representation of a JSON answer object into the
    corresponding command's appropriate representation.
    """

    def _result_and_state(self):
        res = self.raw_result()
        if 'error' in res:
            msg = res['error']['message']
            if 'data' in res['error']:
                msg += " " + str(res['error']['data'])
            raise ArgoException(msg)
        elif 'result' in res:
            return (res['result']['answer'], res['result']['state'])

    def process_result(self, result):
        """Subclasses should override this, to transform a JSON-encoded result
        into the application-specific result.
        """
        raise NotImplementedError('process_result')

    def state(self):
        """Return the protocol state after the command is complete."""
        return self._result_and_state()[1]

    def result(self):
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

    def state(self):
        """Return the state prior to the query, because queries don't change the
        state.
        """
        return self.init_state

    def _result(self):
        res = self.raw_result()
        if 'error' in res:
            msg = res['error']['message']
            if 'data' in res['error']:
                msg += " " + str(res['error']['data'])
            raise ArgoException(msg)
        elif 'result' in res:
            return res['result']['answer']

    def process_result(self, result):
        """Subclasses should override this, to transform a JSON-encoded result
        into the application-specific result.
        """
        raise NotImplementedError('process_result')


    def result(self):
        """Return the result of the query."""
        return self.process_result(self._result())
