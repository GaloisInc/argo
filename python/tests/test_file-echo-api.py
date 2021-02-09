import os
from pathlib import Path
import requests
import subprocess
import time
import json
import unittest
import atexit

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


class GenericFileEchoTests():


    # to be implemented by classes extending this one
    def get_connection(self): pass
    def get_caching_iterations(self): pass

    def test_basics(self):
        c = self.get_connection()
        ## Positive tests -- make sure the server behaves as we expect with valid RPCs

        # Check that their is nothing to show if we haven't loaded a file yet
        uid = c.send_query("show", {"state": None})
        actual = c.wait_for_reply_to(uid)
        expected = show_res(content='',uid=uid,state=None)
        self.assertEqual(actual, expected)
        prev_uid = uid

        # load a file
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

        # check the contents of the loaded file
        uid = c.send_query("show", {"state": hello_state})
        actual = c.wait_for_reply_to(uid)
        expected = show_res(content='Hello World!\n',uid=uid,state=hello_state)
        self.assertEqual(actual, expected)
        self.assertNotEqual(uid, prev_uid)
        prev_uid = uid

        # check a _portion_ of the contents of the loaded file
        uid = c.send_query("show", {"start":1, "end":5, "state": hello_state})
        actual = c.wait_for_reply_to(uid)
        self.assertEqual(actual, show_res(content='ello',uid=uid,state=hello_state))
        self.assertNotEqual(uid, prev_uid)
        prev_uid = uid

        # clear the loaded file
        uid = c.send_command("clear", {"state": hello_state})
        actual = c.wait_for_reply_to(uid)
        self.assertTrue('result' in actual and 'state' in actual['result'])
        cleared_state = actual['result']['state']
        expected = {'result':{'state':cleared_state,'stdout':'','stderr':'','answer':[]},'jsonrpc':'2.0','id':uid}
        self.assertEqual(actual, expected)
        self.assertNotEqual(cleared_state, hello_state)
        self.assertNotEqual(uid, prev_uid)

        # check that the file contents cleared
        uid = c.send_query("show", {"state": cleared_state})
        actual = c.wait_for_reply_to(uid)
        self.assertEqual(actual, show_res(content='',uid=uid,state=cleared_state))

        ## Negative tests -- make sure the server errors as we expect

        # Method not found
        uid = c.send_command("bad function", {"state": cleared_state})
        actual = c.wait_for_reply_to(uid)
        expected = {'error':{'data':{'stdout':None,'data':'bad function','stderr':None},'code':-32601,'message':'Method not found'},'jsonrpc':'2.0','id':uid}
        self.assertEqual(actual, expected)

        # Invalid params
        uid = c.send_command("load", {"file path": 12345, "state": cleared_state})
        actual = c.wait_for_reply_to(uid)
        expected = {'error':{'data':{'stdout':'','data':{'state':cleared_state,'file path':12345},'stderr':''},'code':-32602,'message':'Invalid params: expected String, but encountered Number'},'jsonrpc':'2.0','id':uid}
        self.assertEqual(actual, expected)

        # load a nonexistent file, check error that is returned
        nonexistent_file = file_dir.joinpath('nonexistent.txt')
        if nonexistent_file.is_file():
            print('ERROR: ' + str(nonexistent_file) + ' was expected to not exist, but it does!')
            assert(False)
        uid = c.send_command("load", {"file path": str(nonexistent_file), "state": cleared_state})
        actual = c.wait_for_reply_to(uid)
        expected = {'error':{'data':{'stdout':'','data':{'path':str(nonexistent_file)},'stderr':''},'code':20051,'message':'File doesn\'t exist: ' + str(nonexistent_file)},'jsonrpc':'2.0','id':uid}
        self.assertEqual(actual, expected)


        # cause server to have an internal error
        uid = c.send_command("implode", {"state": cleared_state})
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

    def test_caching(self):
        c = self.get_connection()
        iterations = self.get_caching_iterations()

        # set cache max size to 10 to start
        c.send_notification("set cache limit", {"state": None, "cache limit": 10})

        # Check that their is nothing to show if we haven't loaded a file yet
        uid = c.send_query("show", {"state": None})
        actual = c.wait_for_reply_to(uid)
        expected = show_res(content='',uid=uid,state=None)
        self.assertEqual(actual, expected)

        # Make a misc state to pin and pin it
        uid = c.send_command("prepend", {"content":"I am in a pinned state", "state": None})
        actual = c.wait_for_reply_to(uid)
        pinned_state = actual['result']['state']
        uid = c.send_query("show", {"state": pinned_state})
        actual = c.wait_for_reply_to(uid)
        expected = show_res(content="I am in a pinned state",uid=uid,state=pinned_state)
        self.assertEqual(actual, expected)

        # pin the state
        c.send_notification("pin state", {"state": pinned_state, "state to pin": pinned_state})

        # Check that the pinned state is reachable
        uid = c.send_query("show", {"state": pinned_state})
        actual = c.wait_for_reply_to(uid)
        expected = show_res(content="I am in a pinned state",uid=uid,state=pinned_state)
        self.assertEqual(actual, expected)

        seen_states = [None]
        expected = ''

        # Basically append 10 'c's onto the empty string in a funny way, and then clear the string, over and over
        # (i.e., make a lot of new states, not all of which should fit in the ephemeral cache)
        for i in range(1,iterations):
            # append "abc" on the left
            uid = c.send_command("prepend", {"content":"abc", "state": seen_states[len(seen_states) - 1]})
            seen_states.append(c.wait_for_reply_to(uid)['result']['state'])
            if i % 10 == 0:
                # clear the string by dropping the "ab" and 10 accumulated "c"s from the front
                expected = ""
                uid = c.send_command("drop", {"count":12, "state": seen_states[len(seen_states) - 1]})
                seen_states.append(c.wait_for_reply_to(uid)['result']['state'])
                uid = c.send_query("show", {"state": seen_states[len(seen_states) - 1]})
                actual = c.wait_for_reply_to(uid)['result']['answer']['value']
                self.assertEqual(expected, actual)
            else:
                uid = c.send_command("drop", {"count":2, "state": seen_states[len(seen_states) - 1]})
                seen_states.append(c.wait_for_reply_to(uid)['result']['state'])
                uid = c.send_query("show", {"state": seen_states[len(seen_states) - 1]})
                actual = c.wait_for_reply_to(uid)['result']['answer']['value']
                # we dropped the "ab", but the "c" remains
                expected += 'c'
                self.assertEqual(expected, actual)
            uid = c.send_query("show", {"state": seen_states[1]})
            actual = c.wait_for_reply_to(uid)['result']['answer']['value']
            self.assertEqual('abc', actual)


        # Check that the initial state is still reachable
        uid = c.send_query("show", {"state": None})
        actual = c.wait_for_reply_to(uid)
        self.assertEqual(actual, show_res(content="",uid=uid,state=None))


        # Check that the last couple states and the one we kept revisiting are still live/reachable
        uid = c.send_query("show", {"state": seen_states[len(seen_states) - 1]})
        actual = c.wait_for_reply_to(uid)['result']['answer']['value']
        self.assertEqual(expected, actual)
        uid = c.send_query("show", {"state": seen_states[len(seen_states) - 2]})
        actual = c.wait_for_reply_to(uid)['result']['answer']['value']
        self.assertEqual('ab' + expected, actual)
        uid = c.send_query("show", {"state": seen_states[len(seen_states) - 3]})
        actual = c.wait_for_reply_to(uid)['result']['answer']['value']
        self.assertEqual(expected[:len(expected) - 1], actual)
        uid = c.send_query("show", {"state": seen_states[1]})
        actual = c.wait_for_reply_to(uid)['result']['answer']['value']
        self.assertEqual('abc', actual)

        # Check that old stale states are no longer reachable in the cache
        for i in range(2,50):
            uid = c.send_query("show", {"state": seen_states[i]})
            actual = c.wait_for_reply_to(uid)
            expected = bad_state_res(uid=uid,state=seen_states[i])
            self.assertEqual(actual, expected)

        # Check that the pinned state is still reachable
        uid = c.send_query("show", {"state": pinned_state})
        actual = c.wait_for_reply_to(uid)
        expected = show_res(content="I am in a pinned state",uid=uid,state=pinned_state)
        self.assertEqual(actual, expected)

        # unpinn the pinned state
        c.send_notification("unpin state", {"state": pinned_state, "state to unpin":pinned_state})
        # create some misc traffic so the now unpinned state goes away
        gen_misc_states(c, 100)

        # send a request with a now invalid state id
        uid = c.send_query("show", {"state": pinned_state})
        actual = c.wait_for_reply_to(uid)
        expected = bad_state_res(uid=uid,state=pinned_state)
        self.assertEqual(actual, expected)

        # make a new pinned state
        uid = c.send_command("prepend", {"content":"I am another pinned state", "state": None})
        pinned_state = c.wait_for_reply_to(uid)['result']['state']
        c.send_notification("pin state", {"state": pinned_state, "state to pin": pinned_state})
        # misc traffic
        gen_misc_states(c, 100)
        # check that it's still pinned
        uid = c.send_query("show", {"state": pinned_state})
        actual = c.wait_for_reply_to(uid)
        self.assertEqual(actual['result']['answer']['value'], "I am another pinned state")

        # unpinn the pinned state by unpinning all pinned states
        c.send_notification("unpin all states", {"state": pinned_state})
        # create some misc traffic so the now unpinned state goes away
        gen_misc_states(c, 100)

        # send a request with a now invalid state id
        uid = c.send_query("show", {"state": pinned_state})
        actual = c.wait_for_reply_to(uid)
        expected = bad_state_res(uid=uid,state=pinned_state)
        self.assertEqual(actual, expected)

        # reduce the cache max size to 0
        c.send_notification("set cache limit", {"state": None, "cache limit": 0})
        
        # Now make sure we can linearly use the states, but anything older than the previous is gone.
        for i in range(1, 20):
            # append "foo" on the left then remove it, over and over to churn through states
            uid = c.send_command("prepend", {"content":"foo", "state": None})
            state1 = c.wait_for_reply_to(uid)['result']['state']
            uid = c.send_query("show", {"state": state1})
            actual = c.wait_for_reply_to(uid)
            self.assertEqual(actual, show_res(content="foo",uid=uid,state=state1))
            uid = c.send_command("drop", {"count":3, "state": state1})
            state2 = c.wait_for_reply_to(uid)['result']['state']
            uid = c.send_query("show", {"state": state2})
            actual = c.wait_for_reply_to(uid)
            self.assertEqual(actual, show_res(content="",uid=uid,state=state2))
            # check that the first state is missing as we expect
            uid = c.send_query("show", {"state": state1})
            actual = c.wait_for_reply_to(uid)
            self.assertEqual(actual, bad_state_res(uid=uid,state=state1))




class RemoteSocketProcessTests(GenericFileEchoTests, unittest.TestCase):

    # to be implemented by classes extending this one
    def get_connection(self):
        return argo.ServerConnection(
                argo.RemoteSocketProcess('localhost', 50005, ipv6=True))

    def get_caching_iterations(self):
        return 100

class DynamicSocketProcessTests(GenericFileEchoTests, unittest.TestCase):

    # to be implemented by classes extending this one
    def get_connection(self):
        return argo.ServerConnection(
                argo.DynamicSocketProcess("cabal v2-exec file-echo-api --verbose=0 -- socket --port 50005"))

    def get_caching_iterations(self):
        return 100

class StdIOProcessTests(GenericFileEchoTests, unittest.TestCase):

    # to be implemented by classes extending this one
    def get_connection(self):
        return argo.ServerConnection(
                argo.StdIOProcess("cabal v2-exec file-echo-api --verbose=0 -- stdio"))

    def get_caching_iterations(self):
        return 100


class HttpTests(GenericFileEchoTests, unittest.TestCase):

    # to be implemented by classes extending this one
    def get_connection(self):
        return argo.ServerConnection(
                argo.HttpProcess(url="http://localhost:8080/"))

    def get_caching_iterations(self):
        return 100

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



class LoadOnLaunchTests(unittest.TestCase):

    # to be implemented by classes extending this one
    def test_load_on_launch(self):
        c_preload = argo.ServerConnection(
                    argo.StdIOProcess("cabal v2-exec file-echo-api --verbose=0 -- stdio --file \"" + str(hello_file) + "\""))
        uid = c_preload.send_query("show", {"state": None})
        actual = c_preload.wait_for_reply_to(uid)
        expected = {'result':{'state':None,'stdout':'','stderr':'','answer':{'value':'Hello World!\n'}},'jsonrpc':'2.0','id':uid}
        self.assertEqual(actual, expected)


if __name__ == '__main__':
    unittest.main()
