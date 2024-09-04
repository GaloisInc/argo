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
import threading
import sys
from typing import Any, Dict, List, IO, Mapping, Optional, Union, TextIO

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
    _logging_dest: Optional[TextIO]


    def __init__(self) -> None:
        """Start the process using the given command.
        """
        self._logging_dest = None
        self.setup()

    @abstractmethod
    def setup(self) -> None:
        """Start a process, if one is not already running."""
        pass

    @abstractmethod
    def get_one_reply(self) -> Optional[str]: pass

    @abstractmethod
    def send_one_message(self, the_message: str, *, expecting_response : bool = True) -> None: pass

    def logging(self, on : bool, *, dest : TextIO = sys.stderr) -> None:
        """Whether to log received and transmitted JSON."""
        if on:
            self._logging_dest = dest
        else:
            self._logging_dest = None

    def _log_tx(self, contents : str) -> None:
        if self._logging_dest:
            self._logging_dest.write("[TX] " + contents.strip() + "\n")

    def _log_rx(self, contents : str) -> None:
        if self._logging_dest:
            self._logging_dest.write("[RX] " + contents.strip() + "\n")


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

    def get_one_reply(self) -> Optional[str]:
        """Return the next message if there is one. Block until the message
        is ready or the socket has closed. Return None if the socket closes
        and there are no buffered messages ready."""
        while True:
            got = netstring.decode(self.buf)
            if got is None:
                arrived = self.socket.recv(4096)
                if arrived == '':
                    return None
                self.buf.extend(arrived)
            else:
                (msg, rest) = got
                self.buf = bytearray(rest)
                self._log_rx(msg)
                return msg

    def send_one_message(self, message: str, expecting_response : bool = True) -> None:
        self._log_tx(message)
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
        self.socket = socket.socket(socket.AF_INET6 if self.ipv6 else socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect((self.host, self.port))

    def get_one_reply(self) -> Optional[str]:
        """Return the next message if there is one. Block until the message
        is ready or the socket has closed. Return None if the socket closes
        and there are no buffered messages ready."""
        while True:
            got = netstring.decode(self.buf)
            if got is None:
                arrived = self.socket.recv(4096)
                if arrived == '':
                    return None
                self.buf.extend(arrived)
            else:
                (msg, rest) = got
                self.buf = bytearray(rest)
                self._log_rx(msg)
                return msg

    def send_one_message(self, message: str, *, expecting_response : bool = True) -> None:
        self._log_tx(message)
        msg_bytes = netstring.encode(message)
        self.socket.send(msg_bytes)


class HttpProcess(ServerProcess):
    """A ``ServerProcess`` that contacts a remote HTTP server.
    """
    buf: bytearray
    socket: socket.socket
    ipv6: bool
    waiting_replies: List[str]
    verify: Union[bool, str]

    def __init__(self, url: str, *, verify : Union[bool, str] = True):
        """
           :param url: The URL to connect to an HTTP server on (e.g. ``"http://localhost:8080/"``).
           :param verify: Determines whether a secure connection should verify the SSL certificates.
                          Corresponds to the ``verify`` keyword parameter on ``requests.post``.
        """
        self.url = url
        self.waiting_replies = []
        self.verify = verify
        super().__init__()

    def setup(self) -> None:
        pass

    def get_one_reply(self) -> Optional[str]:
        if len(self.waiting_replies) == 0:
            return None
        else:
            return self.waiting_replies.pop()

    def send_one_message(self, message: str, *, expecting_response : bool = True) -> None:
        self._log_tx(message)
        r = requests.post(self.url,
                          headers={'Content-Type': 'application/json', 'Accept': 'application/json'},
                          data=message,
                          verify=self.verify).text
        self._log_rx(r)
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

    def setup(self) -> None:
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
        self._log_tx(the_message)
        if self.proc is not None and self.proc.stdin is not None:
            self.proc.stdin.write(netstring.encode(the_message))
            self.proc.stdin.flush()
        else:
            raise TypeError("Not a process, or no stdin")

    def get_one_reply(self) -> Optional[str]:
        """If a complete reply has been buffered, parse it from the buffer and
           return it as a bytestring."""
        try:
            msg = self.__messages.get(timeout=0.1)
            self._log_rx(msg)
            return msg
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

    def send_command(self, method: str, params: dict, *, timeout : Optional[float] = None) -> int:
        """Send a message to the server with the given JSONRPC command
           method and parameters. The return value is the unique request
           ID that was used for the message, which can be used to find
           replies.

           A timeout (in seconds) may be specified for the request.
        """
        request_id = self.get_id()
        msg = {'jsonrpc': '2.0',
               'method': method,
               'id': request_id,
               'params': params}
        if timeout:
            msg['timeout'] = round(timeout * 1_000_000)
        msg_string = json.dumps(msg)
        self.process.send_one_message(msg_string)
        return request_id

    def send_query(self, method: str, params: dict, timeout : Optional[float] = None) -> int:
        """Send a message to the server with the given JSONRPC query
           method and parameters. The return value is the unique request
           ID that was used for the message, which can be used to find
           replies.

           A timeout (in seconds) may be specified for the request.
        """
        return self.send_command(method, params, timeout=timeout)

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

        if request_id in self.replies:
            return self.replies.pop(request_id)

        while True:
            reply_bytes = self.process.get_one_reply()
            if reply_bytes is not None:
                the_reply = json.loads(reply_bytes)
                if the_reply['id'] == request_id:
                    return the_reply
                else:
                    self.replies[the_reply['id']] = the_reply

    def logging(self, on : bool, *, dest : TextIO = sys.stderr) -> None:
        """Whether to log received and transmitted JSON."""
        self.process.logging(on, dest = dest)
