import base64
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

def extend_hex(string):
    if len(string) % 2 == 1:
        return '0' + string
    else:
        return string

def fail_with(x):
    "Raise an exception. This is valid in expression positions."
    raise x

# TODO Make this more Pythonic by testing for method support rather
# than instances
def to_cryptol_arg(val):
    if isinstance(val, bool):
        return val
    elif val == ():
        return {'expression': 'unit'}
    elif isinstance(val, tuple):
        return {'expression': 'tuple',
                'data': [to_cryptol_arg(x) for x in val]}
    elif isinstance(val, dict):
        return {'expression': 'record',
                'data': {k : to_cryptol_arg(val[k])
                         if isinstance(k, str)
                         else fail_with (TypeError("Record keys must be strings"))
                         for k in val}}
    elif isinstance(val, int):
        return val
    elif isinstance(val, list):
        return {'expression': 'sequence',
                'data': [to_cryptol_arg(v) for v in val]}
    elif isinstance(val, bytes) or isinstance(val, bytearray):
        return {'expression': 'bits',
                'encoding': 'base64',
                'width': 8 * len(val),
                'data': base64.b64encode(val).decode('ascii')}
    else:
        raise TypeError("Unsupported value")

def from_cryptol_arg(val):
    if isinstance(val, bool):
        return val
    elif isinstance(val, int):
        return val
    elif 'expression' in val.keys():
        tag = val['expression']
        if tag == 'unit':
            return ()
        elif tag == 'tuple':
            return (from_cryptol_arg(x) for x in val['data'])
        elif tag == 'record':
            return {k : from_cryptol_arg(val[k]) for k in val['data']}
        elif tag == 'sequence':
            return [from_cryptol_arg(v) for v in val['data']]
        elif tag == 'bits':
            enc = val['encoding']
            if enc == 'base64':
                data = base64.b64decode(val['data'].encode('ascii'))
            elif enc == 'hex':
                data = bytes.fromhex(extend_hex(val['data']))
            else:
                raise ValueError("Unknown encoding " + str(enc))
            return data
        else:
            raise ValueError("Unknown expression tag " + tag)
    else:
        raise TypeError("Unsupported value")


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
                msg = self.the_response['error']['message']
                if 'data' in self.the_response['error']:
                    msg += " " + str(self.the_response['error']['data'])
                raise CryptolException(msg)
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

class ChangeDirectoryResult(CryptolResult):
    def process_result(self, result):
        return result

class LoadFileResult(CryptolResult):
    def process_result(self, result):
        return result

class EvalExprResult(CryptolResult):
    def process_result(self, result):
        return result['answer'][0]

    def __str__(self):
        return str(self.result())

class CallResult(CryptolResult):
    def process_result(self, result):
        return from_cryptol_arg(result['answer']['value'])




class CryptolConnection(object):
    def __init__(self, port):
        self.port = port
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.connect(("127.0.0.1", port))
        self.sock.setblocking(False)
        self.buf = bytearray(b'')
        self.replies = {}
        self.next_id = 0

        self.most_recent_result = None

    def snapshot(self):
        snap = CryptolConnection(self.port)
        snap.sock = self.sock
        snap.buf = self.buf
        snap.replies = self.replies
        snap.next_id = self.next_id
        snap.most_recent_result = self.most_recent_result

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
                #self.sock.setblocking(True)
                self.process_replies()
            finally:
                self.sock.setblocking(False)
        return self.replies[request_id]

    def protocol_state(self):
        if self.most_recent_result is None:
            return []
        else:
            return self.most_recent_result.result_state()

    # Protocol messages
    def change_directory(self, new_directory):
        request_id = self.send_message('change directory',
                                       {'state': self.protocol_state(), 'directory': new_directory})
        self.most_recent_result = ChangeDirectoryResult(self, request_id)
        return self.most_recent_result

    def load_file(self, filename):
        request_id = self.send_message('load module', {'state': self.protocol_state(), 'file': filename})
        self.most_recent_result = LoadFileResult(self, request_id)
        return self.most_recent_result

    def evaluate_expression(self, expression):
        request_id = self.send_message('evaluate expression', {'state': self.protocol_state(), 'expression': expression})
        self.most_recent_result = EvalExprResult(self, request_id)
        return self.most_recent_result

    def call(self, fun, *args):
        encoded_args = [to_cryptol_arg(a) for a in args]
        request_id = self.send_message('call', {'state': self.protocol_state(),
                                                'function': fun,
                                                'arguments': encoded_args})
        self.most_recent_result = CallResult(self, request_id)
        return self.most_recent_result
