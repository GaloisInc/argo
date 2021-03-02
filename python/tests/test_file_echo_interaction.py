import os
import unittest
import argo_client.interaction as argo
from pathlib import Path
from argo_client.interaction import HasProtocolState, ArgoException
from argo_client.connection import ServerConnection, StdIOProcess
from typing import Any, Optional

class LoadFile(argo.Command):
    def __init__(self, connection : HasProtocolState, file_path : str) -> None:
        super(LoadFile, self).__init__('load', {'file path': file_path}, connection)

    def process_result(self, res : Any) -> Any:
        return res

class Implode(argo.Command):
    def __init__(self, connection : HasProtocolState) -> None:
        super(Implode, self).__init__('implode', {}, connection)

    def process_result(self, res : Any) -> Any:
        return res

class Show(argo.Query):
    def __init__(self, connection : HasProtocolState, start : Optional[int], end : Optional[int]) -> None:
        params = {'state': connection.protocol_state()}
        if start is not None:
            params['start'] += start
        if end is not None:
            params['end'] += end
        super(Show, self).__init__('show', params, connection)

    def process_result(self, res : Any) -> Any:
        return res['value']

class Reset(argo.Notification):
    def __init__(self, connection : HasProtocolState) -> None:
        super(Reset, self).__init__('destroy state', {'state to destroy': connection.protocol_state()}, connection)


class FileEchoConnection:
    most_recent_result : Optional[argo.Interaction]
    server_connection : ServerConnection

    def __init__(self, connection : ServerConnection):
        self.most_recent_result = None
        self.server_connection = connection

    def protocol_state(self) -> Any:
        if self.most_recent_result is None:
            return None
        else:
            return self.most_recent_result.state()

    
    def load_file(self, filename : str) -> argo.Command:
        """Load a file's contents into the server.
        """
        self.most_recent_result = LoadFile(self, filename)
        return self.most_recent_result

    def implode(self) -> argo.Command:
        """Cause an internal server error.
        """
        self.most_recent_result = Implode(self)
        return self.most_recent_result

    def show(self, *, start : int = None, end : int = None) -> argo.Query:
        """Load a file's contents into the server.
        """
        self.most_recent_result = Show(self, start = start, end = end)
        return self.most_recent_result

    def reset(self) -> None:
        """Reset the underlying server state."""
        Reset(self)
        self.most_recent_result = None

dir_path = Path(os.path.dirname(os.path.realpath(__file__)))
file_dir = dir_path.joinpath('test-data')
if not file_dir.is_dir():
    print('ERROR: ' + str(file_dir) + ' is not a directory!')
    assert(False)

class BasicInteractionTests(unittest.TestCase):
    # Connection to server
    c : FileEchoConnection = None

    @classmethod
    def setUpClass(self):
        self.c = FileEchoConnection(
                    argo.ServerConnection(
                        StdIOProcess(
                            "cabal run exe:file-echo-api --verbose=0 -- stdio")))

    def test_basics(self):
        c = self.c
        hello_file = file_dir.joinpath('hello.txt')
        self.assertTrue(False if not hello_file.is_file() else True)

        # test loading and showing a valid file
        c.load_file(str(hello_file))
        self.assertEqual(c.show().result(), "Hello World!\n")

        c.reset()


class CommandErrorInteractionTests1(unittest.TestCase):
    # Connection to server
    c : FileEchoConnection = None

    @classmethod
    def setUpClass(self):
        self.c = FileEchoConnection(
                    argo.ServerConnection(
                        StdIOProcess(
                            "cabal run exe:file-echo-api --verbose=0 -- stdio")))

    def test_missing_file(self):
        c = self.c

        # test loading a non-existant file
        halo_file = file_dir.joinpath('halo.txt') # <- file doesn't exist
        self.assertFalse(False if not halo_file.is_file() else True)
        with self.assertRaises(ArgoException):
            c.load_file(str(halo_file)).result()

        # test that internal errors without extra data raise proper exceptions
        with self.assertRaises(ArgoException):
            c.implode().result()


class CommandErrorInteractionTests2(unittest.TestCase):
    # Connection to server
    c : FileEchoConnection = None

    @classmethod
    def setUpClass(self):
        self.c = FileEchoConnection(
                    argo.ServerConnection(
                        StdIOProcess(
                            "cabal run exe:file-echo-api --verbose=0 -- stdio")))

    def test_implosion_after_load(self):
        c = self.c

        hello_file = file_dir.joinpath('hello.txt')
        self.assertTrue(False if not hello_file.is_file() else True)

        # test loading and showing a valid file
        c.load_file(str(hello_file))
        self.assertEqual(c.show().result(), "Hello World!\n")

        # test that internal errors without extra data raise proper exceptions
        with self.assertRaises(ArgoException):
            c.implode().result()


class CommandErrorInteractionTests3(unittest.TestCase):
    # Connection to server
    c : FileEchoConnection = None

    @classmethod
    def setUpClass(self):
        self.c = FileEchoConnection(
                    argo.ServerConnection(
                        StdIOProcess(
                            "cabal run exe:file-echo-api --verbose=0 -- stdio")))

    def test_only_implision(self):
        c = self.c

        # test that internal errors without extra data raise proper exceptions
        with self.assertRaises(ArgoException):
            c.implode().result()


class CommandErrorInteractionTests4(unittest.TestCase):
    # Connection to server
    c : FileEchoConnection = None

    @classmethod
    def setUpClass(self):
        self.c = FileEchoConnection(
                    argo.ServerConnection(
                        StdIOProcess(
                            "cabal run exe:file-echo-api --verbose=0 -- stdio")))

    def test_load_after_reset(self):
        c = self.c

        hello_file = file_dir.joinpath('hello.txt')
        self.assertTrue(False if not hello_file.is_file() else True)

        # test loading and showing a valid file
        c.load_file(str(hello_file))
        self.assertEqual(c.show().result(), "Hello World!\n")

        c.reset()

        # post reset connection is in initial state
        self.assertEqual(c.show().result(), "")

        # test loading and showing a valid file after a reset
        base_file = file_dir.joinpath('base.txt')
        self.assertTrue(False if not base_file.is_file() else True)
        c.load_file(str(base_file))
        self.assertEqual(c.show().result(), "All your base are belong to us!\n")
