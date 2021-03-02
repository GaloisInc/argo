import os
from pathlib import Path
import subprocess
import time
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

    def assertShow(self, connection : argo.ServerConnection, state : Optional[str], expected : str) -> Tuple[int, str]:
        """Send a `show` command from `state` and ensure it returns `expected`.
        
        Returns the request uid and state in a tuple."""

        next_state = None
        uid = connection.send_query("show", {"state": state})
        actual = connection.wait_for_reply_to(uid)
        self.assertIn('result', actual)
        if 'result' in actual:
            self.assertIn('state', actual['result'])
            if 'state' in actual['result']:
                next_state = actual['result']['state']
            self.assertIn('answer', actual['result'])
            if 'answer' in actual['result']:
                self.assertIn('value', actual['result']['answer'])
                if 'value' in actual['result']['answer']:
                    self.assertEqual(actual['result']['answer']['value'], expected)
        
        return (uid, next_state)


    def test_subsequent_connection(self):
        c = argo.ServerConnection(argo.HttpProcess('http://localhost:8080/'))
        ## Positive tests -- make sure the server behaves as we expect with valid RPCs

        # [c] Check that their is nothing to show if we haven't loaded a file yet
        (prev_uid, state) = self.assertShow(c, state=None,expected='')

        # [c] load a file
        hello_file = file_dir.joinpath('hello.txt')
        self.assertTrue(False if not hello_file.is_file() else True)
        uid = c.send_command("load", {"file path": str(hello_file), "state": state})
        actual = c.wait_for_reply_to(uid)
        self.assertTrue('result' in actual and 'state' in actual['result'])
        state = actual['result']['state']
        expected = {'result':{'state':state,'stdout':'','stderr':'','answer':[]},'jsonrpc':'2.0','id':uid}
        self.assertEqual(actual, expected)
        self.assertNotEqual(uid, prev_uid)
        self.assertNotEqual(state, None)
        prev_uid = uid

        # [c] check the contents of the loaded file
        (prev_uid, state) = self.assertShow(c, state=state,expected='Hello World!\n')

        # [other_c] start a subsequent connection
        other_c = argo.ServerConnection(argo.HttpProcess('http://localhost:8080/'))

        # [other_c] check that the other connection has nothing to show if we haven't loaded our own file yet
        (other_prev_uid, other_state) = self.assertShow(other_c, state=None,expected='')


        # [other_c] load a file
        base_file = file_dir.joinpath('base.txt')
        other_uid = other_c.send_command("load", {"file path": str(base_file), "state": other_state})
        actual = other_c.wait_for_reply_to(other_uid)
        self.assertTrue('result' in actual and 'state' in actual['result'])
        other_state = actual['result']['state']
        expected = {'result':{'state':other_state,'stdout':'','stderr':'','answer':[]},'jsonrpc':'2.0','id':other_uid}
        self.assertEqual(actual, expected)
        self.assertNotEqual(other_uid, other_prev_uid)
        self.assertNotEqual(other_state, None)
        other_prev_uid = other_uid

        # [other_c] clear the loaded file
        other_uid = other_c.send_command("clear", {"state": other_state})
        actual = other_c.wait_for_reply_to(other_uid)
        self.assertTrue('result' in actual and 'state' in actual['result'])
        cleared_state = actual['result']['state']
        expected = {'result':{'state':cleared_state,'stdout':'','stderr':'','answer':[]},'jsonrpc':'2.0','id':other_uid}
        self.assertEqual(actual, expected)
        self.assertNotEqual(cleared_state, other_state)
        self.assertNotEqual(other_uid, other_prev_uid)

        # [c] check the contents of the loaded file again in the original connection
        (prev_uid, state) = self.assertShow(c, state=state,expected='Hello World!\n')

