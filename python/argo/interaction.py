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
