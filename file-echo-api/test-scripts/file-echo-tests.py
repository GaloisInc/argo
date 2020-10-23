import os
from pathlib import Path
import signal
import subprocess
import sys
import time

import argo.connection as argo

dir_path = Path(os.path.dirname(os.path.realpath(__file__)))

file_dir = dir_path.joinpath('test-data')

if not file_dir.is_dir():
    print('ERROR: ' + str(file_dir) + ' is not a directory!')
    assert(False)


def run_tests(c):
    ## Positive tests -- make sure the server behaves as we expect with valid RPCs

    # Check that their is nothing to show if we haven't loaded a file yet
    uid = c.send_message("show", {"state": None})
    actual = c.wait_for_reply_to(uid)
    expected = {'result':{'state':None,'stdout':'','stderr':'','answer':{'value':''}},'jsonrpc':'2.0','id':uid}
    assert(actual == expected)
    prev_uid = uid

    # load a file
    hello_file = file_dir.joinpath('hello.txt')
    assert(False if not hello_file.is_file() else True)
    uid = c.send_message("load", {"file path": str(hello_file), "state": None})
    actual = c.wait_for_reply_to(uid)
    assert('result' in actual and 'state' in actual['result'])
    hello_state = actual['result']['state']
    expected = {'result':{'state':hello_state,'stdout':'','stderr':'','answer':[]},'jsonrpc':'2.0','id':uid}
    assert(actual == expected)
    assert(uid != prev_uid)
    assert(hello_state != None)
    prev_uid = uid

    # check the contents of the loaded file
    uid = c.send_message("show", {"state": hello_state})
    actual = c.wait_for_reply_to(uid)
    expected = {'result':{'state':hello_state,'stdout':'','stderr':'','answer':{'value':'Hello World!\n'}},'jsonrpc':'2.0','id':uid}
    assert(actual == expected)
    assert(uid != prev_uid)
    prev_uid = uid

    # check a _portion_ of the contents of the loaded file
    uid = c.send_message("show", {"start":1, "end":5, "state": hello_state})
    actual = c.wait_for_reply_to(uid)
    expected = {'result':{'state':hello_state,'stdout':'','stderr':'','answer':{'value':'ello'}},'jsonrpc':'2.0','id':uid}
    assert(actual == expected)
    assert(uid != prev_uid)
    prev_uid = uid

    # clear the loaded file
    uid = c.send_message("clear", {"state": hello_state})
    actual = c.wait_for_reply_to(uid)
    assert('result' in actual and 'state' in actual['result'])
    cleared_state = actual['result']['state']
    expected = {'result':{'state':cleared_state,'stdout':'','stderr':'','answer':[]},'jsonrpc':'2.0','id':uid}
    assert(actual == expected)
    assert(cleared_state != hello_state)
    assert(uid != prev_uid)

    # check that the file contents cleared
    uid = c.send_message("show", {"state": cleared_state})
    actual = c.wait_for_reply_to(uid)
    expected = {'result':{'state':cleared_state,'stdout':'','stderr':'','answer':{'value':''}},'jsonrpc':'2.0','id':uid}
    assert(actual == expected)

    ## Negative tests -- make sure the server errors as we expect

    # Method not found
    uid = c.send_message("bad function", {"state": cleared_state})
    actual = c.wait_for_reply_to(uid)
    expected = {'error':{'data':{'stdout':'','data':'bad function','stderr':''},'code':-32601,'message':'Method not found'},'jsonrpc':'2.0','id':uid}
    assert(actual == expected)

    # Invalid params
    uid = c.send_message("load", {"file path": 12345, "state": cleared_state})
    actual = c.wait_for_reply_to(uid)
    expected = {'error':{'data':{'stdout':'','data':{'state':cleared_state,'file path':12345},'stderr':''},'code':-32602,'message':'Invalid params: expected String, but encountered Number'},'jsonrpc':'2.0','id':uid}
    assert(actual == expected)

    # load a nonexistent file, check error that is returned
    nonexistent_file = file_dir.joinpath('nonexistent.txt')
    if nonexistent_file.is_file():
        print('ERROR: ' + str(nonexistent_file) + ' was expected to not exist, but it does!')
        assert(False)
    uid = c.send_message("load", {"file path": str(nonexistent_file), "state": cleared_state})
    actual = c.wait_for_reply_to(uid)
    expected = {'error':{'data':{'stdout':'','data':{'path':str(nonexistent_file)},'stderr':''},'code':20051,'message':'File doesn\'t exist: ' + str(nonexistent_file)},'jsonrpc':'2.0','id':uid}
    assert(actual == expected)

# Test with both sockets and stdio
env = os.environ.copy()

# Launch a separate process for the RemoteSocketProcess test
p = subprocess.Popen(
    ["cabal", "v2-exec", "file-echo-api", "--verbose=0", "--", "--port", "50005"],
    stdout=subprocess.DEVNULL,
    stdin=subprocess.DEVNULL,
    stderr=subprocess.DEVNULL,
    start_new_session=True,
    env=env)

time.sleep(5)
assert(p is not None)
# assert(p.poll() is None)

# Test argo's RemoteSocketProcess
c = argo.ServerConnection(
       argo.RemoteSocketProcess('localhost', 50005, ipv6=True))

run_tests(c)

# close the remote process, we don't need it for the remaining tests
os.killpg(os.getpgid(p.pid), signal.SIGKILL)

# Test argo's DynamicSocketProcess
c = argo.ServerConnection(
       argo.DynamicSocketProcess("cabal v2-exec file-echo-api --verbose=0 -- --port 50005"))

run_tests(c)

c = argo.ServerConnection(
       argo.StdIOProcess("cabal v2-exec file-echo-api --verbose=0 -- --stdio"))

run_tests(c)
