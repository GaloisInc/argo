import socket
import json

# Current status:
#  It can currently connect to a server over a socket. Try the following:
#  >>> c = CryptolConnection(PORT)
#  >>> f = c.load_file(FILE)
#  >>> f.result()
#
# TODO:
#  1. State tracking across calls
#  2. Allow multiple replies to messages for intermediate status updates
#  3. Implement the rest of the methods

def encode(string):
    bytestring = string.encode()
    return str(len(bytestring)).encode() + b':' + bytestring + b','

def decode(netstring):
    i = 0
    length_bytes = bytearray(b'')
    while chr(netstring[i]).isdigit():
        length_bytes.append(netstring[i])
        i += 1
    if chr(netstring[i]).encode() != b':':
        raise ValueError("Malformed netstring, missing :")
    length = int(length_bytes.decode())
    i += 1
    out = bytearray(b'')
    for j in range(0, length):
        out.append(netstring[i])
        i += 1
    if chr(netstring[i]).encode() != b',':
        raise ValueError("Malformed netstring, missing ,")
    i += 1
    return (out.decode(), netstring[i:])

class CryptolException(Exception):
    pass

class CryptolResult(object):
    def __init__(self, connection, request_id):
        self.connection = connection
        self.request_id = request_id
        self.the_response = None
        self.the_state = None
        self.the_result = None

    def ready(self):
        if self.the_response is None:
            self.connection.process_replies()
            return self.request_id in self.connection.replies
        else:
            return True

    def raw_result_and_state(self):
        if self.the_response is None:
            self.the_response = self.connection.wait_for_reply_to(self.request_id)

        if self.the_state is None:
            if 'error' in self.the_response:
                raise CryptolException(reply['error']['message'])
            elif 'result' in self.the_response:

                self.the_state = self.the_response['result'].pop('state')
                return (self.the_response['result'], self.the_state)
        else:
            return (self.the_result, self.the_state)

    def result(self):
        return self.process_result(self.raw_result_and_state()[0])

    def result_state(self):
        return self.raw_result_and_state()[1]

    def process_result(self, result):
        """Override this method to interpret the answer"""
        raise NotImplementedError()


class LoadFileResult(CryptolResult):
    def process_result(self, result):
        return result

class CryptolConnection(object):
    def __init__(self, port):
        self.port = port
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.connect(("127.0.0.1", port))
        self.sock.setblocking(False)
        self.buf = bytearray(b'')
        self.replies = {}
        self.next_id = 0

    def get_id(self):
        the_id = self.next_id
        self.next_id += 1
        return the_id

    def buffer_replies(self):
        try:
            arrived = self.sock.recv(4096)
            while arrived != b'':
                self.buf.extend(arrived)
                arrived = self.sock.recv(4096)
            return None
        except BlockingIOError:
            return None

    def get_one_reply(self):
        try:
            (msg, rest) = decode(self.buf)
            self.buf = rest
            return msg
        except (ValueError, IndexError):
            return None

    def process_replies(self):
        self.buffer_replies()
        r = self.get_one_reply()
        while r is not None:
            the_reply = json.loads(r)
            self.replies[the_reply['id']] = the_reply
            r = self.get_one_reply()

    def send_message(self, method, params):
        request_id = self.get_id()
        msg = {'jsonrpc':'2.0',
               'method': method,
               'id': request_id,
               'params': params}
        msg_string = json.dumps(msg)
        msg_bytes = encode(msg_string)
        self.sock.send(msg_bytes)
        return request_id

    def wait_for_reply_to(self, request_id):
        self.process_replies()
        while request_id not in self.replies:
            try:
                self.sock.setblocking(True)
                self.process_replies()
            finally:
                self.sock.setblocking(False)
        return self.replies[request_id]

    # Protocol messages
    def load_file(self, filename):
        request_id = self.send_message('load module', {'state': [], 'file': filename})
        return LoadFileResult(self, request_id)
