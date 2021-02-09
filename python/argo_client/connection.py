"""Utilities for connecting to Argo-based servers."""

from __future__ import annotations

from abc import ABCMeta, abstractmethod
import json
import os
import queue
import re
import requests
import socket
import subprocess
import signal
import sys
import threading
import time
from typing import Any, Dict, List, IO, Mapping, Optional, Union

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


class ServerProcess(metaclass=ABCMeta):
    """A wrapper around a server process.

       These is used by :py:class:`ServerConnection`. Each connection
       has a process to which it can send and receive messages.

       Each subclass should:

       1. Implement ``setup`` to launch a process, create a socket, or
          whatever else is necessary to communicate with the server.

       2. Implement ``get_one_reply``, so that it returns a string
          that contains a JSON message if one is available, or ``None``
          if not.

       3. Implement ``send_one_message``, which sends a string that
          contains a serialized JSON message to the process.
    """


    def __init__(self) -> None:
        """Start the process using the given command.
        """
        self.setup()

    @abstractmethod
    def setup(self) -> None:
        """Start a process, if one is not already running."""
        pass

    @abstractmethod
    def get_one_reply(self) -> Optional[str]: pass

    @abstractmethod
    def send_one_message(self, the_message: str, *, expecting_response : bool = True) -> None: pass

class ManagedProcess(ServerProcess, metaclass=ABCMeta):
    """A ``ServerProcess`` that is responsible for starting and stopping
    the underlying server, as well as buffering I/O to and from the server.
    """
    buf: bytearray
    proc: Optional[subprocess.Popen]

    def __init__(self, command: str, *,
                 environment: Optional[Union[Mapping[bytes, Union[bytes, str]],
                                             Mapping[str, Union[bytes, str]]]]=None):
        """ Construct a managed process by executing the given command.

            :param command: The command to be executed in :func:`setup`.
            :param environment: A process environment to be used instead of the default.
        """
        self.command = command
        self.environment_override = environment
        self.buf = bytearray(b'')
        self.proc = None

        super().__init__()

    def pid(self) -> Optional[int]:
        """Return the process group id of the managed server process."""
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
            try:
                os.killpg(os.getpgid(self.proc.pid), signal.SIGKILL)
            except ProcessLookupError:
                pass


class SocketProcess(ManagedProcess):
    """A ``ServerProcess`` whose process communicates over a socket.
    """
    port: Optional[int]
    socket: socket.socket
    environment_override: Optional[Union[Mapping[bytes, Union[bytes, str]],
                                         Mapping[str, Union[bytes, str]]]]

    def __init__(self, command: str, *,
                 persist: bool=False,
                 environment: Optional[Union[Mapping[bytes, Union[bytes, str]],
                                             Mapping[str, Union[bytes, str]]]]=None):
        """:param command: The command to be executed, using a shell,
             to start the server.

           :param persist: Whether to allow the subprocess to survive
             the Python process. If this is ``False``, the subprocess is
             killed when no longer needed.

           :param environment: The environment in which to execute the
             server (if ``None``, the environemnt of the Python process is
             used).
        """
        self.persist = persist
        super().__init__(command, environment=environment)


    def buffer_replies(self) -> None:
        """Read any replies that the server has sent, and add their byte
           representation to the internal buffer, freeing up space in
           the pipe or socket.
        """
        try:
            arrived = self.socket.recv(4096)
            while arrived != b'':
                self.buf.extend(arrived)
                arrived = self.socket.recv(4096)
            return None
        except BlockingIOError:
            return None

    def get_one_reply(self) -> Optional[str]:
        """If a complete reply has been buffered, parse it from the buffer and
           return it as a bytestring."""
        self.buffer_replies()
        try:
            (msg, rest) = netstring.decode(self.buf)
            self.buf = bytearray(rest)
            return msg
        except (ValueError, IndexError):
            return None

    def send_one_message(self, message: str, expecting_response : bool = True) -> None:
        msg_bytes = netstring.encode(message)
        self.socket.send(msg_bytes)

    def __del__(self) -> None:
        if self.proc is not None:
            if not self.persist:
                try:
                    os.killpg(os.getpgid(self.proc.pid), signal.SIGKILL)
                except ProcessLookupError:
                    pass



class DynamicSocketProcess(SocketProcess):
    """A ``SocketServerProcess`` whose process communicates over a socket
    on a port chosen arbitrarily by the process itself. This port
    should be written to ``stdout`` after the literal string ``PORT``.
    """

    def setup(self) -> None:
        super().setup()
        if self.proc is None or self.proc.poll() is not None:
            # To debug, consider setting stderr to sys.stdout instead (to see
            # server log messages).
            self.proc = subprocess.Popen(
                self.command,
                shell=True,
                stdout=subprocess.PIPE,
                # stderr=sys.stdout,
                stderr=subprocess.DEVNULL,
                env=self.environment_override,
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

        self.socket = socket.socket(socket.AF_INET6, socket.SOCK_STREAM)
        self.socket.connect(("localhost", self.port))
        self.socket.setblocking(False)




class RemoteSocketProcess(ServerProcess):
    """A ``ServerProcess`` whose process communicates over a socket
    on a given port.
    """
    buf: bytearray
    socket: socket.socket
    ipv6: bool

    def __init__(self, host: str, port: int, ipv6: bool=True):
        """
           :param host: The hostname to connect to (e.g. ``"localhost"``)
           :param port: The port on which to connect
           :param ipv6: Whether to use IPv6 (``False`` for IPv4)
        """
        self.host = host
        self.port = port
        self.ipv6 = ipv6
        self.buf = bytearray(b'')
        super().__init__()

    def setup(self) -> None:
        super().setup()

        self.socket = socket.socket(socket.AF_INET6 if self.ipv6 else socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect((self.host, self.port))
        self.socket.setblocking(False)

    def buffer_replies(self) -> None:
        """Read any replies that the server has sent, and add their byte
           representation to the internal buffer, freeing up space in
           the pipe or socket.
        """
        try:
            arrived = self.socket.recv(4096)
            while arrived != b'':
                self.buf.extend(arrived)
                arrived = self.socket.recv(4096)
            return None
        except BlockingIOError:
            return None

    def get_one_reply(self) -> Optional[str]:
        """If a complete reply has been buffered, parse it from the buffer and
           return it as a bytestring."""
        self.buffer_replies()
        try:
            (msg, rest) = netstring.decode(self.buf)
            self.buf = bytearray(rest)
            return msg
        except (ValueError, IndexError):
            return None

    def send_one_message(self, message: str, *, expecting_response : bool = True) -> None:
        msg_bytes = netstring.encode(message)
        self.socket.send(msg_bytes)


class HttpProcess(ServerProcess):
    """A ``ServerProcess`` that contacts a remote HTTP server.
    """
    buf: bytearray
    socket: socket.socket
    ipv6: bool
    waiting_replies: List[str]

    def __init__(self, url: str):
        """
           :param host: The hostname to connect to (e.g. ``"localhost"``)
           :param port: The port on which to connect
           :param path: The path to send requests to
        """
        self.url = url
        self.waiting_replies = []
        super().__init__()

    def setup(self) -> None:
        pass

    def get_one_reply(self) -> Optional[str]:
        if len(self.waiting_replies) == 0:
            return None
        else:
            return self.waiting_replies.pop()

    def send_one_message(self, message: str, *, expecting_response : bool = True) -> None:
        r = requests.post(self.url,
                          headers={'Content-Type': 'application/json', 'Accept': 'application/json'},
                          data=message).text
        if expecting_response:
            self.waiting_replies.append(r)



def enqueue_netstring(out: IO[bytes], queue: queue.Queue[str]) -> None:
    while True:
        length_bytes = bytearray(b'')
        b = out.read(1)
        while chr(b[0]).isdigit():
            length_bytes.append(b[0])
            b = out.read(1)
        length = int(length_bytes.decode())
        message = out.read(length).decode()
        queue.put(message)
        out.read(1) # comma

class StdIOProcess(ManagedProcess):
    """A ``SocketServerProcess`` whose process communicates over ``stdin``
    and ``stdout``.
    """
    __messages: queue.Queue[str] # multiprocessing.Queue[str] #
    __proc_thread: threading.Thread #multiprocessing.Process #

    def setup(self) -> None:
        super().setup()
        if self.proc is None or self.proc.poll() is not None:
            # To debug, consider setting stderr to sys.stdout instead (to see
            # server log messages).
            self.proc = subprocess.Popen(
                self.command,
                shell=True,
                text=False,
                stdout=subprocess.PIPE,
                stdin=subprocess.PIPE,
                #stderr=sys.stdout,
                stderr=subprocess.DEVNULL,
                bufsize=0,
                start_new_session=True,
                env=self.environment_override)

            if self.proc.stdout is None:
                raise ValueError("Server process has no stdout")


            self.__messages = queue.Queue()
            self.proc_thread = threading.Thread(target=enqueue_netstring, args=(self.proc.stdout, self.__messages))
            # self.proc_thread = multiprocessing.Process(target=enqueue_netstring, args=(self.proc.stdout, self.__messages))
            self.proc_thread.daemon = True
            self.proc_thread.start()

    def send_one_message(self, the_message: str, *, expecting_response : bool = True) -> None:
        if self.proc is not None and self.proc.stdin is not None:
            self.proc.stdin.write(netstring.encode(the_message))
            self.proc.stdin.flush()
        else:
            raise TypeError("Not a process, or no stdin")

    def get_one_reply(self) -> Optional[str]:
        """If a complete reply has been buffered, parse it from the buffer and
           return it as a bytestring."""
        try:
            return self.__messages.get(timeout=0.1)
        except queue.Empty:
            return None



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

        self.replies = {}
        self.ids = IDSource()

    def get_id(self) -> int:
        """Return a fresh request ID that has not previously been used with
           this connection."""
        return self.ids.get()

    def _process_replies(self) -> None:
        """Remove all pending replies from the internal buffer, parse them
           into JSON, and add them to the internal collection of replies.
        """
        reply_bytes = self.process.get_one_reply()
        while reply_bytes is not None:
            the_reply = json.loads(reply_bytes)
            self.replies[the_reply['id']] = the_reply
            reply_bytes = self.process.get_one_reply()

    def send_command(self, method: str, params: dict) -> int:
        """Send a message to the server with the given JSONRPC command
           method and parameters. The return value is the unique request
           ID that was used for the message, which can be used to find
           replies.
        """
        request_id = self.get_id()
        msg = {'jsonrpc': '2.0',
               'method': method,
               'id': request_id,
               'params': params}
        msg_string = json.dumps(msg)
        self.process.send_one_message(msg_string)
        return request_id

    def send_query(self, method: str, params: dict) -> int:
        """Send a message to the server with the given JSONRPC query
           method and parameters. The return value is the unique request
           ID that was used for the message, which can be used to find
           replies.
        """
        return self.send_command(method, params)

    def send_notification(self, method: str, params: dict) -> None:
        """Send a message to the server with the given JSONRPC notification
           method and parameters. There is no return value, since notifications
           do not allow for a response from the server.
        """
        msg = {'jsonrpc': '2.0',
               'method': method,
               'params': params}
        msg_string = json.dumps(msg)
        self.process.send_one_message(msg_string, expecting_response = False)

    def wait_for_reply_to(self, request_id: int) -> Any:
        """Block until a reply is received for the given
           ``request_id``. Return the reply."""
        self._process_replies()
        while request_id not in self.replies:
            self._process_replies()

        return self.replies[request_id] #self.replies.pop(request_id)  # delete reply while returning it
