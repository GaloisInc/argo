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


def assertShow(self : unittest.TestCase,
               connection : argo.ServerConnection,
               state : Optional[str],
               expected : str,
               *,
               expected_equal : bool = True) -> Tuple[int, str]:
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
                if expected_equal:
                    self.assertEqual(actual['result']['answer']['value'], expected)
                else:
                    self.assertNotEqual(actual['result']['answer']['value'], expected)
    return (uid, next_state)


class MutableFileEchoTests(unittest.TestCase):

    @classmethod
    def setUpClass(self):
        p = subprocess.Popen(
            ["cabal", "run", "exe:mutable-file-echo-api", "--verbose=0", "--", "--max-occupancy", "2", "http", "/", "--port", "8080"],
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
        (prev_uid, state) = assertShow(self, c, state=None,expected='')

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
        (prev_uid, state) = assertShow(self, c, state=state,expected='Hello World!\n')

        # [other_c] start a subsequent connection
        other_c = argo.ServerConnection(argo.HttpProcess('http://localhost:8080/'))

        # [other_c] check that the other connection has nothing to show if we haven't loaded our own file yet
        (other_prev_uid, other_state) = assertShow(self, other_c, state=None,expected='')


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
        (prev_uid, state) = assertShow(self, c, state=state,expected='Hello World!\n')


class InterruptTests(unittest.TestCase):
    # Connection to server
    c = None
    # process running the server
    p = None

    @classmethod
    def setUpClass(self):
        p = subprocess.Popen(
            ["cabal", "run", "exe:mutable-file-echo-api", "--verbose=0", "--", "--max-occupancy", "2", "http", "/", "--port", "8083", "--file", str(hello_file)],
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
    def test_interrupts(self):
        c1 = argo.ServerConnection(argo.HttpProcess('http://localhost:8083/'))
        c2 = argo.ServerConnection(argo.HttpProcess('http://localhost:8083/'))
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

        # simple sleep for 3 seconds
        t1 = time.time()
        uid1 = c1.send_query("sleep query", {"microseconds": 3000000, "state": state1})
        actual1 = c1.wait_for_reply_to(uid1)
        t2 = time.time()
        self.assertIn('result', actual1)
        if 'result' in actual1:
            self.assertIn('state', actual1['result'])
            self.assertIn('answer', actual1['result'])
            if 'answer' in actual1['result']:
                self.assertIn('value', actual1['result']['answer'])
                if 'value' in actual1['result']['answer']:
                    self.assertGreater(actual1['result']['answer']['value'], 2.9)
        self.assertGreater(t2 - t1, 2.9)


        # sleep/interrupt test
        newpid = os.fork()
        if newpid != 0:
            # parent tries to sleep
            one_hundred_sec = 100000000
            t1 = time.time()
            uid1 = c1.send_query("sleep query", {"microseconds": one_hundred_sec, "state": state1})
            t2 = time.time()
        else:
            # child does not allow sleep
            time.sleep(3)
            uid2 = c2.send_notification("interrupt", {})
            os._exit(0)
        # check interrupt actually interrupted 100sec sleep
        self.assertLess(t2 - t1, 10)

        # mutation/interrupt test
        newpid = os.fork()
        if newpid != 0:
            # parent tries to clean up
            two_sec = 2000000
            t1 = time.time()
            uid1 = c1.send_command("slow clear", {"pause microseconds": two_sec, "state": state1})
        else:
            # child allows cleanup to start but not finish
            time.sleep(4)
            uid2 = c2.send_notification("interrupt", {})
            os._exit(0)

        # check contents (N.B., the mutable server uses an IORef for the
        # contents, so the interrupt doesn't prevent effects.)
        (uid1, state1) = assertShow(self, c1, state=state1, expected='Hello World!\n', expected_equal = False)
        (uid1, state1) = assertShow(self, c1, state=state1, expected='', expected_equal = False)
        (uid2, state2) = assertShow(self, c2, state=state2, expected='Hello World!\n')
