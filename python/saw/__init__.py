import argo
import argo.connection as ac
import argo.interaction
from saw.commands import *

import weakref

def connect(command, cryptol_path=None):
    proc = ac.ServerProcess(command)
    conn = ac.ServerConnection(proc)
    return SAWConnection(conn)


class SAWConnection:
    """A representation of a current user state in a session with SAW."""
    def __init__(self, server_connection):
        self.most_recent_result = None
        self.server_connection = server_connection

    def snapshot(self):
        """Return a ``SAWConnection`` that has the same process and state as
        the current connection. The new connection's state will be
        independent of the current state.
        """
        copy = SAWConnection(self.server_connection)
        copy.most_recent_result = self.most_recent_result
        return copy

    def protocol_state(self):
        if self.most_recent_result is None:
            return []
        else:
            return self.most_recent_result.state()

    # Protocol messages
    def cryptol_start_setup(self, name):
        self.most_recent_result = CryptolStartSetup(self, name)
        return self.most_recent_result

    def cryptol_load_file(self, filename):
        self.most_recent_result = CryptolLoadFile(self, filename)
        return self.most_recent_result

    def cryptol_finish_setup(self):
        self.most_recent_result = CryptolFinishSetup(self)
        return self.most_recent_result
