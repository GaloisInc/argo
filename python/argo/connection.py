"""Utilities for connecting to Argo-based servers."""

import json
import os
import re
import socket
import subprocess
import signal
from typing import Any, Dict, Mapping, Optional, Union

from . import netstring


# Must be boxed separately to enable sharing of connections
class IDSource:  # pylint: disable=too-few-public-methods
    """A source of unique identifiers for JSON RPC requests."""

    next_id: int

    def __init__(self) -> None:
        self.next_id = 0

    def get(self) -> int:
        """Get a unique (for this instance) number."""
        self.next_id += 1
        return self.next_id


class ServerProcess:
    """A wrapper around a server process, responsible for setting it up and
       killing it when finished."""

    proc: Optional[subprocess.Popen]

    def __init__(self, command: str, *, persist: bool=False) -> None:
        """Start the process using the given command.

           :param command: The command to be executed, using a shell, to start
           the server.
        """
        self.command = command
        self.persist = persist
        self.proc = None
        self.setup()

    def get_environment(self) -> Optional[Union[Mapping[bytes, Union[bytes, str]],
                                                Mapping[str, Union[bytes, str]]]]:
        """Return the environment in which the server process should be
           started. By default, this is the Override this method to
           allow customization. If ``None`` is returned, then the
           Python process's environment is used.
        """
        return None

    def setup(self) -> None:
        """Start a process, if one is not already running, using the
           environment determined by ``get_environment``."""
        if self.proc is None or self.proc.poll() is not None:
            # To debug, consider setting stderr to sys.stdout instead (to see
            # server log messages).
            self.proc = subprocess.Popen(
                self.command,
                shell=True,
                stdout=subprocess.PIPE,
                # stderr=sys.stdout,
                stderr=subprocess.DEVNULL,
                env=self.get_environment(),
                start_new_session=True,
                universal_newlines=True)

            if self.proc.stdout is None:
                raise ValueError("Server process has no stdout")
            out_line = self.proc.stdout.readline()

            match = re.match(r'PORT (\d+)', out_line)
            if match:
                self.port = int(match.group(1))
            else:
                raise Exception("Failed to load process, output was `" +
                                out_line + "' but expected PORT then a port.")

    def pid(self) -> Optional[int]:
        """Return the process group id of the managed server process"""
        if self.proc is not None:
            return os.getpgid(self.proc.pid)
        else:
            return None

    def running(self) -> bool:
        """Check whether the process is still running."""
        if self.proc is not None and self.proc.poll() is None:
            return True
        else:
            return False

    def __del__(self) -> None:
        if self.proc is not None:
            if not self.persist:
                try:
                    os.killpg(os.getpgid(self.proc.pid), signal.SIGKILL)
                except ProcessLookupError:
                    pass


class ServerConnection:
    """A ``ServerConnection`` represents a logical connection to a
       server. Because the Argo protocols represent all state
       explicitly, a connection is not necessarily tied to a single
       process.

       More specifically, the ``ServerConnection`` maintains a source
       of unique request IDs and a mapping from these IDs to the
       answer received (if any). Furthermore, it has a means of
       sending messages to the server.
    """
    replies: Dict[int, Any]

    def __init__(self, process: ServerProcess) -> None:
        """:param process: The ``ServerProcess`` used for the connection."""
        self.process = process
        self.port = self.process.port

        self.sock = socket.socket(socket.AF_INET6, socket.SOCK_STREAM)
        self.sock.connect(("localhost", self.port))
        self.sock.setblocking(False)
        self.buf = bytearray(b'')
        self.replies = {}
        self.ids = IDSource()

    def get_id(self) -> int:
        """Return a fresh request ID that has not previously been used with
           this connection."""
        return self.ids.get()

    def _buffer_replies(self) -> None:
        """Read any replies that the server has sent, and add their byte
           representation to the internal buffer, freeing up space in
           the pipe or socket.
        """
        try:
            arrived = self.sock.recv(4096)
            while arrived != b'':
                self.buf.extend(arrived)
                arrived = self.sock.recv(4096)
            return None
        except BlockingIOError:
            return None

    def _get_one_reply(self) -> Optional[str]:
        """If a complete reply has been buffered, parse it from the buffer and
           return it as a bytestring."""
        try:
            (msg, rest) = netstring.decode(self.buf)
            self.buf = bytearray(rest)
            return msg
        except (ValueError, IndexError):
            return None

    def _process_replies(self) -> None:
        """Remove all pending replies from the internal buffer, parse them
           into JSON, and add them to the internal collection of replies.
        """
        self._buffer_replies()
        reply_bytes = self._get_one_reply()
        while reply_bytes is not None:
            the_reply = json.loads(reply_bytes)
            self.replies[the_reply['id']] = the_reply
            reply_bytes = self._get_one_reply()

    def send_message(self, method: str, params: dict) -> int:
        """Send a message to the server with the given JSONRPC method and
           parameters. The return value is the unique request ID that
           was used for the message, which can be used to find
           replies.
        """
        request_id = self.get_id()
        msg = {'jsonrpc': '2.0',
               'method': method,
               'id': request_id,
               'params': params}
        msg_string = json.dumps(msg)
        msg_bytes = netstring.encode(msg_string)
        self.sock.send(msg_bytes)
        return request_id

    def wait_for_reply_to(self, request_id: int) -> Any:
        """Block until a reply is received for the given
           ``request_id``. Return the reply."""
        self._process_replies()
        while request_id not in self.replies:
            try:
                # self.sock.setblocking(True)
                self._process_replies()
            finally:
                self.sock.setblocking(False)
        return self.replies.pop(request_id)  # delete reply whilst returning it
