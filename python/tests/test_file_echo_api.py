import os
from pathlib import Path
import requests
import subprocess
import time
import json
import unittest
import signal
from typing import Optional, Tuple

import argo_client.connection as argo

dir_path = Path(os.path.dirname(os.path.realpath(__file__)))

file_dir = dir_path.joinpath('test-data')

if not file_dir.is_dir():
    print('ERROR: ' + str(file_dir) + ' is not a directory!')
    assert(False)

# Test the custom command line argument to load a file at server start
hello_file = file_dir.joinpath('hello.txt')

# What the response looks like when a state is not in cache/pinned on the server
def bad_state_res(*,uid,state):
    r = {'error':{'data':{'stdout':None,'data':state,'stderr':None},'code':20,'message':'Unknown state ID'},'jsonrpc':'2.0','id':uid}
    return r

# Helper to churn through states on the server
def gen_misc_states(c, iterations):
    state = None
    for i in range (1,iterations):
        # append "foo" on the left then remove it, over and over to churn through states
        uid = c.send_command("prepend", {"content":"foo", "state": state})
        state = c.wait_for_reply_to(uid)['result']['state']
        uid = c.send_command("drop", {"count":3, "state": state})
        state = c.wait_for_reply_to(uid)['result']['state']

class GenericFileEchoTests():


    # to be implemented by classes extending this one
    def get_connection(self): pass
    def get_caching_iterations(self): pass

    def assertShow(
        self : unittest.TestCase,
        connection : argo.ServerConnection,
        state : Optional[str],
        expected : str,
        *,
        startEnd : Optional[Tuple[int,int]] = None) -> int:
        """Send a `show` command from `state` and ensure it returns `expected`.
        
        Returns the request uid and state in a tuple."""

        next_state = None
        if startEnd is None:
            uid = connection.send_query("show", {"state": state})
        else:
            (start, end) = startEnd
            uid = connection.send_query("show", {"start":start, "end":end, "state": state})
        actual = connection.wait_for_reply_to(uid)
        self.assertIn('result', actual)
        if 'result' in actual:
            self.assertIn('state', actual['result'])
            self.assertIn('answer', actual['result'])
            if 'answer' in actual['result']:
                self.assertIn('value', actual['result']['answer'])
                if 'value' in actual['result']['answer']:
                    self.assertEqual(actual['result']['answer']['value'], expected)
        
        return uid

    def test_basics(self):
        c = self.get_connection()
        ## Positive tests -- make sure the server behaves as we expect with valid RPCs

        # Check that their is nothing to show if we haven't loaded a file yet
        prev_uid = self.assertShow(c, state=None, expected='')
        prev_state = None

        # load a file
        hello_file = file_dir.joinpath('hello.txt')
        self.assertTrue(False if not hello_file.is_file() else True)
        uid = c.send_command("load", {"file path": str(hello_file), "state": prev_state})
        actual = c.wait_for_reply_to(uid)
        self.assertTrue('result' in actual and 'state' in actual['result'])
        state = actual['result']['state']
        expected = {'result':{'state':state,'stdout':'','stderr':'','answer':[]},'jsonrpc':'2.0','id':uid}
        self.assertEqual(actual, expected)
        self.assertNotEqual(uid, prev_uid)
        self.assertNotEqual(state, prev_state)
        prev_uid = uid
        prev_state = state

        # check the contents of the loaded file
        prev_uid = self.assertShow(c, state=prev_state, expected='Hello World!\n')

        # check a _portion_ of the contents of the loaded file
        prev_uid = self.assertShow(c, state=prev_state, expected='ello', startEnd=(1,5))

        # clear the loaded file
        uid = c.send_command("clear", {"state": prev_state})
        actual = c.wait_for_reply_to(uid)
        self.assertTrue('result' in actual and 'state' in actual['result'])
        state = actual['result']['state']
        expected = {'result':{'state':state,'stdout':'','stderr':'','answer':[]},'jsonrpc':'2.0','id':uid}
        self.assertEqual(actual, expected)
        self.assertNotEqual(state, prev_state)
        self.assertNotEqual(uid, prev_uid)
        prev_uid = uid
        prev_state = state

        # check that the file contents cleared
        prev_uid = self.assertShow(c, state=prev_state, expected='')

        ## Negative tests -- make sure the server errors as we expect

        # Method not found
        uid = c.send_command("bad function", {"state": prev_state})
        actual = c.wait_for_reply_to(uid)
        expected = {'error':{'data':{'stdout':None,'data':'bad function','stderr':None},'code':-32601,'message':'Method not found'},'jsonrpc':'2.0','id':uid}
        self.assertEqual(actual, expected)

        # Invalid params
        uid = c.send_command("load", {"file path": 12345, "state": prev_state})
        actual = c.wait_for_reply_to(uid)
        expected = {'error':{'data':{'stdout':'','data':{'state':prev_state,'file path':12345},'stderr':''},'code':-32602,'message':'Invalid params: expected String, but encountered Number'},'jsonrpc':'2.0','id':uid}
        self.assertEqual(actual, expected)

        # load a nonexistent file, check error that is returned
        nonexistent_file = file_dir.joinpath('nonexistent.txt')
        if nonexistent_file.is_file():
            print('ERROR: ' + str(nonexistent_file) + ' was expected to not exist, but it does!')
            assert(False)
        uid = c.send_command("load", {"file path": str(nonexistent_file), "state": prev_state})
        actual = c.wait_for_reply_to(uid)
        expected = {'error':{'data':{'stdout':'','data':{'path':str(nonexistent_file)},'stderr':''},'code':20051,'message':'File doesn\'t exist: ' + str(nonexistent_file)},'jsonrpc':'2.0','id':uid}
        self.assertEqual(actual, expected)


        # cause server to have an internal error
        uid = c.send_command("implode", {"state": prev_state})
        actual = c.wait_for_reply_to(uid)
        expected = {'error':{'data':{'stdout':'','stderr':''},'code':-32603,'message':'Internal error'},'jsonrpc':'2.0','id':uid}
        self.assertEqual(actual, expected)

        # send a request with an invalid state id
        uid = c.send_query("show", {"state": "12345678-9101-1121-3141-516171819202"})
        actual = c.wait_for_reply_to(uid)
        expected = {'error':{'data':{'stdout':None,'data':'12345678-9101-1121-3141-516171819202','stderr':None},'code':20,'message':'Unknown state ID'},'jsonrpc':'2.0','id':uid}
        self.assertEqual(actual, expected)

        # invalid request (missing JSON-RPC required fields)
        invalid_request = {'jsonrpc': '2.0','method': 'bad request', 'id':1}
        c.process.send_one_message(json.dumps(invalid_request))
        actual = c.wait_for_reply_to(None)
        expected = {'error':{'data':{'stdout':None,'data':'Error in $: key \"params\" not found','stderr':None},'code':-32700,'message':'Parse error'},'jsonrpc':'2.0','id':None}
        self.assertEqual(actual, expected)

        # invalid request (Bad JSON)
        invalid_request = "BAAAAAD JSON"
        c.process.send_one_message(invalid_request)
        time.sleep(2) # pause before fetching response so we don't just read the previous response whose JSON-RPC id is `null`
        actual = c.wait_for_reply_to(None)
        expected = {'error':{'data':{'stdout':None,'data':'Error in $: Failed reading: not a valid json value at \'BAAAAADJSON\'','stderr':None},'code':-32700,'message':'Parse error'},'jsonrpc':'2.0','id':None}
        self.assertEqual(actual, expected)


class RemoteSocketProcessTests(GenericFileEchoTests, unittest.TestCase):
    # Connection to cryptol
    c = None
    # process running the server
    p = None

    @classmethod
    def setUpClass(self):
        p = subprocess.Popen(
            ["cabal", "run", "exe:file-echo-api", "--verbose=0", "--", "socket", "--port", "50005"],
            stdout=subprocess.PIPE,
            stdin=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
            start_new_session=True)
        time.sleep(3)
        assert(p is not None)
        poll_result = p.poll()
        if poll_result is not None:
            print(poll_result)
            print(p.stdout.read())
            print(p.stderr.read())
        assert(poll_result is None)

        self.p = p
        self.c = argo.ServerConnection(argo.RemoteSocketProcess('localhost', 50005, ipv6=True))


    @classmethod
    def tearDownClass(self):
        os.killpg(os.getpgid(self.p.pid), signal.SIGKILL)
        super().tearDownClass()


    # to be implemented by classes extending this one
    def get_connection(self):
        return self.c

    def get_caching_iterations(self):
        return 30

class DynamicSocketProcessTests(GenericFileEchoTests, unittest.TestCase):
    # Connection to server
    c = None

    @classmethod
    def setUpClass(self):
        self.c = argo.ServerConnection(
                    argo.DynamicSocketProcess("cabal run exe:file-echo-api --verbose=0 -- socket --port 50006"))

    # to be implemented by classes extending this one
    def get_connection(self):
        return self.c

    def get_caching_iterations(self):
        return 30

class StdIOProcessTests(GenericFileEchoTests, unittest.TestCase):
    # Connection to server
    c = None

    @classmethod
    def setUpClass(self):
        self.c = argo.ServerConnection(
                    argo.StdIOProcess("cabal run exe:file-echo-api --verbose=0 -- stdio"))

    def get_connection(self):
        return self.c

    def get_caching_iterations(self):
        return 30


class HttpTests(GenericFileEchoTests, unittest.TestCase):
    # Connection to server
    c = None
    # process running the server
    p = None

    @classmethod
    def setUpClass(self):
        p = subprocess.Popen(
            ["cabal", "run", "exe:file-echo-api", "--verbose=0", "--", "http", "/", "--port", "8080"],
            stdout=subprocess.PIPE,
            stdin=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
            start_new_session=True)
        time.sleep(3)
        assert(p is not None)
        poll_result = p.poll()
        if poll_result is not None:
            print(poll_result)
            print(p.stdout.read())
            print(p.stderr.read())
        assert(poll_result is None)

        self.p = p
        self.c = argo.ServerConnection(argo.HttpProcess('http://localhost:8080/'))


    @classmethod
    def tearDownClass(self):
        os.killpg(os.getpgid(self.p.pid), signal.SIGKILL)
        super().tearDownClass()

    def get_connection(self):
        return self.c

    def get_caching_iterations(self):
        return 30

    def test_http_behaviors(self):
        ### Additional tests for the HTTP server ###

        # Ensure that only POST is allowed to the designated URL
        get_response = requests.get("http://localhost:8080/")
        self.assertEqual(get_response.status_code, 405)

        # Ensure that other URLs give 404
        get_response = requests.get("http://localhost:8080/some/other/resource")
        self.assertEqual(get_response.status_code, 404)

        post_response = requests.post("http://localhost:8080/some/other/resource")
        self.assertEqual(post_response.status_code, 404)

        # Wrong content-type
        post_response = requests.post("http://localhost:8080/")
        self.assertEqual(post_response.status_code, 415)

        good_headers = {'Content-Type': 'application/json', 'Accept': 'application/json'}

        # Parse error for request body
        post_response = requests.post("http://localhost:8080/", headers=good_headers)
        self.assertEqual(post_response.status_code, 400)

        post_response = requests.request('POST', "http://localhost:8080/", headers=good_headers, data='{"id":0, "jsonrpc": "2.0", "method": "clear", "params":{"state": null}}')
        self.assertEqual(post_response.status_code, 200)


class TLSTests(GenericFileEchoTests, unittest.TestCase):
    # Connection to server
    c = None
    # process running the server
    p = None

    @classmethod
    def setUpClass(self):
        os.system('openssl req -nodes -newkey rsa:2048 -keyout server.key -out server.csr'\
                  + ' -subj "/C=GB/ST=London/L=London/O=Acme Widgets/OU=IT Department/CN=localhost"')
        os.system('openssl x509 -req -days 365 -in server.csr -signkey server.key -out server.crt')
        server_env = os.environ.copy()
        server_env["TLS_ENABLE"] = "1"
        p = subprocess.Popen(
            ["cabal", "run", "exe:file-echo-api", "--verbose=0", "--", "http", "/", "--port", "8083"],
            stdout=subprocess.PIPE,
            stdin=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
            start_new_session=True,
            env=server_env)
        time.sleep(3)
        assert(p is not None)
        poll_result = p.poll()
        if poll_result is not None:
            print(poll_result)
            print(p.stdout.read())
            print(p.stderr.read())
        assert(poll_result is None)

        self.p = p
        self.c = argo.ServerConnection(argo.HttpProcess('https://localhost:8083', verify=False))


    @classmethod
    def tearDownClass(self):
        os.killpg(os.getpgid(self.p.pid), signal.SIGKILL)
        super().tearDownClass()

    def get_connection(self):
        return self.c

    def get_caching_iterations(self):
        return 30


class LoadOnLaunchTests(unittest.TestCase):
    # Connection to server
    c = None
    # process running the server
    p = None

    @classmethod
    def setUpClass(self):
        p = subprocess.Popen(
            ["cabal", "run", "exe:file-echo-api", "--verbose=0", "--", "http", "/", "--port", "8081", "--file", str(hello_file)],
            stdout=subprocess.PIPE,
            stdin=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
            start_new_session=True)
        time.sleep(3)
        assert(p is not None)
        poll_result = p.poll()
        if poll_result is not None:
            print(poll_result)
            print(p.stdout.read())
            print(p.stderr.read())
        assert(poll_result is None)

        self.p = p
        self.c = argo.ServerConnection(argo.HttpProcess('http://localhost:8081/'))


    @classmethod
    def tearDownClass(self):
        os.killpg(os.getpgid(self.p.pid), signal.SIGKILL)
        super().tearDownClass()

    # to be implemented by classes extending this one
    def test_load_on_launch(self):
        uid = self.c.send_query("show", {"state": None})
        actual = self.c.wait_for_reply_to(uid)
        self.assertIn('result', actual)
        self.assertIn('state', actual['result'])
        expected = {'result':{'state':actual['result']['state'],'stdout':'','stderr':'','answer':{'value':'Hello World!\n'}},'jsonrpc':'2.0','id':uid}
        self.assertEqual(actual, expected)


class OccupancyTests(unittest.TestCase):
    # Connection to server
    c = None
    # process running the server
    p = None

    @classmethod
    def setUpClass(self):
        p = subprocess.Popen(
            ["cabal", "run", "exe:file-echo-api", "--verbose=0", "--", "--max-occupancy", "2", "http", "/", "--port", "8082", "--file", str(hello_file)],
            stdout=subprocess.PIPE,
            stdin=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
            start_new_session=True)
        time.sleep(3)
        assert(p is not None)
        poll_result = p.poll()
        if poll_result is not None:
            print(poll_result)
            print(p.stdout.read())
            print(p.stderr.read())
        assert(poll_result is None)

        self.p = p

    @classmethod
    def tearDownClass(self):
        os.killpg(os.getpgid(self.p.pid), signal.SIGKILL)
        super().tearDownClass()

    # to be implemented by classes extending this one
    def test_occupancy_and_state_destruction(self):
        c1 = argo.ServerConnection(argo.HttpProcess('http://localhost:8082/'))
        c2 = argo.ServerConnection(argo.HttpProcess('http://localhost:8082/'))
        # load a file
        hello_file = file_dir.joinpath('hello.txt')
        self.assertTrue(False if not hello_file.is_file() else True)
        uid1 = c1.send_command("load", {"file path": str(hello_file), "state": None})
        uid2 = c2.send_command("load", {"file path": str(hello_file), "state": None})
        actual1 = c1.wait_for_reply_to(uid1)
        self.assertIn('result', actual1)
        self.assertIn('state', actual1['result'])
        state1 = actual1['result']['state']
        actual2 = c2.wait_for_reply_to(uid2)
        self.assertIn('result', actual2)
        self.assertIn('state', actual2['result'])
        state2 = actual2['result']['state']

        # check the next (3rd) connection is rejected
        c3 = argo.ServerConnection(argo.HttpProcess('http://localhost:8082/'))
        uid3 = c3.send_command("load", {"file path": str(hello_file), "state": None})
        actual3 = c3.wait_for_reply_to(uid3)
        expected = {'error':{'data':{'stdout':None,'stderr':None},'code':22,'message':'Server at max capacity'},'jsonrpc':'2.0','id':uid3}
        self.assertEqual(actual3, expected)

        # kill connection 1's state
        c1.send_notification("destroy state", {"state to destroy": state1})
        # ensure connection 1's state is dead
        uid1 = c1.send_query("show", {"state": state1})
        actual1 = c1.wait_for_reply_to(uid1)
        expected = {'error':{'data':{'stdout':None,'data':state1,'stderr':None},'code':20,'message':'Unknown state ID'},'jsonrpc':'2.0','id':uid1}
        self.assertEqual(actual1, expected)

        # now connection 3 should succeed
        uid3 = c3.send_command("load", {"file path": str(hello_file), "state": None})
        actual3 = c3.wait_for_reply_to(uid3)
        self.assertIn('result', actual3)
        self.assertIn('state', actual3['result'])
        state3 = actual3['result']['state']

        # kill all the connection's state
        c1.send_notification("destroy all states", {})

        # ensure connection 2's state is dead
        uid2 = c2.send_query("show", {"state": state2})
        actual2 = c2.wait_for_reply_to(uid2)
        expected = {'error':{'data':{'stdout':None,'data':state2,'stderr':None},'code':20,'message':'Unknown state ID'},'jsonrpc':'2.0','id':uid2}
        self.assertEqual(actual2, expected)

        # ensure connection 3's state is dead
        uid3 = c3.send_query("show", {"state": state3})
        actual3 = c3.wait_for_reply_to(uid3)
        expected = {'error':{'data':{'stdout':None,'data':state3,'stderr':None},'code':20,'message':'Unknown state ID'},'jsonrpc':'2.0','id':uid3}
        self.assertEqual(actual3, expected)

        # now ensure there's room for two more new connections
        uid1 = c1.send_command("load", {"file path": str(hello_file), "state": None})
        uid2 = c2.send_command("load", {"file path": str(hello_file), "state": None})
        actual1 = c1.wait_for_reply_to(uid1)
        self.assertIn('result', actual1)
        self.assertIn('state', actual1['result'])
        state1 = actual1['result']['state']
        actual2 = c2.wait_for_reply_to(uid2)
        self.assertIn('result', actual2)
        self.assertIn('state', actual2['result'])
        state2 = actual2['result']['state']

        # check again that the next (3rd) connection is rejected
        c3 = argo.ServerConnection(argo.HttpProcess('http://localhost:8082/'))
        uid3 = c3.send_command("load", {"file path": str(hello_file), "state": None})
        actual3 = c3.wait_for_reply_to(uid3)
        expected = {'error':{'data':{'stdout':None,'stderr':None},'code':22,'message':'Server at max capacity'},'jsonrpc':'2.0','id':uid3}
        self.assertEqual(actual3, expected)

