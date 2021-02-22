import os
from pathlib import Path
import subprocess
import time
import unittest
import signal

import argo_client.connection as argo

dir_path = Path(os.path.dirname(os.path.realpath(__file__)))

file_dir = dir_path.joinpath('test-data')

if not file_dir.is_dir():
    print('ERROR: ' + str(file_dir) + ' is not a directory!')
    assert(False)

# Test the custom command line argument to load a file at server start
hello_file = file_dir.joinpath('hello.txt')

# What the response to "show" looks like
def show_res(*,content,uid,state):
    r = {'result':{'state':state,'stdout':'','stderr':'','answer':{'value':content}},'jsonrpc':'2.0','id':uid}
    return r

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


class MutableFileEchoTests(unittest.TestCase):

    @classmethod
    def setUpClass(self):
        p = subprocess.Popen(
            ["cabal", "run", "exe:mutable-file-echo-api", "--verbose=0", "--", "http", "/", "--port", "8080"],
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
        self.other_c = argo.ServerConnection(argo.HttpProcess('http://localhost:8080/'))


    @classmethod
    def tearDownClass(self):
        os.killpg(os.getpgid(self.p.pid), signal.SIGKILL)
        super().tearDownClass()

    def test_subsequent_connection(self):
        c = argo.ServerConnection(argo.HttpProcess('http://localhost:8080/'))
        ## Positive tests -- make sure the server behaves as we expect with valid RPCs

        # [c] Check that their is nothing to show if we haven't loaded a file yet
        uid = c.send_query("show", {"state": None})
        actual = c.wait_for_reply_to(uid)
        expected = show_res(content='',uid=uid,state=None)
        self.assertEqual(actual, expected)
        prev_uid = uid

        # [c] load a file
        hello_file = file_dir.joinpath('hello.txt')
        self.assertTrue(False if not hello_file.is_file() else True)
        uid = c.send_command("load", {"file path": str(hello_file), "state": None})
        actual = c.wait_for_reply_to(uid)
        self.assertTrue('result' in actual and 'state' in actual['result'])
        hello_state = actual['result']['state']
        expected = {'result':{'state':hello_state,'stdout':'','stderr':'','answer':[]},'jsonrpc':'2.0','id':uid}
        self.assertEqual(actual, expected)
        self.assertNotEqual(uid, prev_uid)
        self.assertNotEqual(hello_state, None)
        prev_uid = uid

        # [c] check the contents of the loaded file
        uid = c.send_query("show", {"state": hello_state})
        actual = c.wait_for_reply_to(uid)
        expected = show_res(content='Hello World!\n',uid=uid,state=hello_state)
        self.assertEqual(actual, expected)
        self.assertNotEqual(uid, prev_uid)
        prev_uid = uid

        # [other_c] start a subsequent connection
        other_c = argo.ServerConnection(argo.HttpProcess('http://localhost:8080/'))

        # [other_c] check that the other connection has nothing to show if we haven't loaded our own file yet
        other_uid = other_c.send_query("show", {"state": None})
        actual = other_c.wait_for_reply_to(other_uid)
        expected = show_res(content='',uid=other_uid,state=None)
        self.assertEqual(actual, expected)
        other_prev_uid = other_uid

        # [other_c] load a file
        base_file = file_dir.joinpath('base.txt')
        other_uid = other_c.send_command("load", {"file path": str(base_file), "state": None})
        actual = other_c.wait_for_reply_to(other_uid)
        self.assertTrue('result' in actual and 'state' in actual['result'])
        base_state = actual['result']['state']
        expected = {'result':{'state':base_state,'stdout':'','stderr':'','answer':[]},'jsonrpc':'2.0','id':other_uid}
        self.assertEqual(actual, expected)
        self.assertNotEqual(other_uid, other_prev_uid)
        self.assertNotEqual(base_state, None)
        other_prev_uid = other_uid

        # [other_c] clear the loaded file
        other_uid = other_c.send_command("clear", {"state": base_state})
        actual = c.wait_for_reply_to(uid)
        self.assertTrue('result' in actual and 'state' in actual['result'])
        cleared_state = actual['result']['state']
        expected = {'result':{'state':cleared_state,'stdout':'','stderr':'','answer':[]},'jsonrpc':'2.0','id':other_uid}
        self.assertEqual(actual, expected)
        self.assertNotEqual(cleared_state, base_state)
        self.assertNotEqual(other_uid, other_prev_uid)

        # [c] check the contents of the loaded file again in the original connection
        uid = c.send_query("show", {"state": hello_state})
        actual = c.wait_for_reply_to(uid)
        expected = show_res(content='Hello World!\n',uid=uid,state=hello_state)
        self.assertEqual(actual, expected)
        self.assertNotEqual(uid, prev_uid)
        prev_uid = uid
