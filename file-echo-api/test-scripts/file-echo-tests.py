import os
from pathlib import Path
import requests
import signal
import subprocess
import sys
import time
import json

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
    expected = {'error':{'data':{'stdout':None,'data':'bad function','stderr':None},'code':-32601,'message':'Method not found'},'jsonrpc':'2.0','id':uid}
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


    # cause server to have an internal error
    uid = c.send_message("implode", {"state": cleared_state})
    actual = c.wait_for_reply_to(uid)
    expected = {'error':{'data':{'stdout':'','stderr':''},'code':-32603,'message':'Internal error'},'jsonrpc':'2.0','id':uid}
    assert(actual == expected)

    # send a request with an invalid state id
    uid = c.send_message("show", {"state": "12345678-9101-1121-3141-516171819202"})
    actual = c.wait_for_reply_to(uid)
    expected = {'error':{'data':{'stdout':None,'data':'12345678-9101-1121-3141-516171819202','stderr':None},'code':20,'message':'Unknown state ID'},'jsonrpc':'2.0','id':uid}
    assert(actual == expected)

    # invalid request (missing JSON-RPC required fields)
    invalid_request = {'jsonrpc': '2.0','method': 'bad request', 'id':1}
    c.process.send_one_message(json.dumps(invalid_request))
    actual = c.wait_for_reply_to(None)
    expected = {'error':{'data':{'stdout':None,'data':'Error in $: key \"params\" not found','stderr':None},'code':-32700,'message':'Parse error'},'jsonrpc':'2.0','id':None}
    assert(actual == expected)

   # invalid request (Bad JSON)
    invalid_request = "BAAAAAD JSON"
    c.process.send_one_message(invalid_request)
    time.sleep(2) # pause before fetching response so we don't just read the previous response whose JSON-RPC id is `null`
    actual = c.wait_for_reply_to(None)
    expected = {'error':{'data':{'stdout':None,'data':'Error in $: Failed reading: not a valid json value at \'BAAAAADJSON\'','stderr':None},'code':-32700,'message':'Parse error'},'jsonrpc':'2.0','id':None}
    assert(actual == expected)


# Test with both sockets and stdio
env = os.environ.copy()

# Launch a separate process for the RemoteSocketProcess test
p = subprocess.Popen(
    ["cabal", "v2-exec", "file-echo-api", "--verbose=0", "--", "socket", "--port", "50005"],
    stdout=subprocess.DEVNULL,
    stdin=subprocess.DEVNULL,
    stderr=subprocess.DEVNULL,
    start_new_session=True,
    env=env)

p_http = subprocess.Popen(
           ["cabal", "v2-exec", "file-echo-api", "--verbose=0", "--", "http", "/", "--port", "8080"],
           stdout=subprocess.DEVNULL,
           stdin=subprocess.DEVNULL,
           stderr=subprocess.DEVNULL,
           start_new_session=True,
           env=env)


time.sleep(5)
assert(p is not None)
assert(p.poll() is None)
assert(p_http is not None)
assert(p_http.poll() is None)

# Test argo's RemoteSocketProcess
c = argo.ServerConnection(
       argo.RemoteSocketProcess('localhost', 50005, ipv6=True))

run_tests(c)

# close the remote process, we don't need it for the remaining tests
os.killpg(os.getpgid(p.pid), signal.SIGKILL)

# Test argo's DynamicSocketProcess
c = argo.ServerConnection(
       argo.DynamicSocketProcess("cabal v2-exec file-echo-api --verbose=0 -- socket --port 50005"))

run_tests(c)

c = argo.ServerConnection(
       argo.StdIOProcess("cabal v2-exec file-echo-api --verbose=0 -- stdio"))

run_tests(c)



c_http = argo.ServerConnection(
            argo.HttpProcess(url="http://localhost:8080/"))

run_tests(c_http)

### Additional tests for the HTTP server ###

# Ensure that only POST is allowed to the designated URL
get_response = requests.get("http://localhost:8080/")
assert(get_response.status_code == 405)

# Ensure that other URLs give 404
get_response = requests.get("http://localhost:8080/some/other/resource")
assert(get_response.status_code == 404)

post_response = requests.post("http://localhost:8080/some/other/resource")
assert(post_response.status_code == 404)

# Wrong content-type
post_response = requests.post("http://localhost:8080/")
assert(post_response.status_code == 415)

good_headers = {'Content-Type': 'application/json', 'Accept': 'application/json'}

# Parse error for request body
post_response = requests.post("http://localhost:8080/", headers=good_headers)
assert(post_response.status_code == 400)

post_response = requests.request('POST', "http://localhost:8080/", headers=good_headers, data='{"id":0, "jsonrpc": "2.0", "method": "clear", "params":{"state": null}}')
assert(post_response.status_code == 200)

os.killpg(os.getpgid(p_http.pid), signal.SIGKILL)

# Test the custom command line argument to load a file at server start
hello_file = file_dir.joinpath('hello.txt')
c_preload = argo.ServerConnection(
              argo.StdIOProcess("cabal v2-exec file-echo-api --verbose=0 -- stdio --file \"" + str(hello_file) + "\""))
uid = c_preload.send_message("show", {"state": None})
actual = c_preload.wait_for_reply_to(uid)
expected = {'result':{'state':None,'stdout':'','stderr':'','answer':{'value':'Hello World!\n'}},'jsonrpc':'2.0','id':uid}
assert(actual == expected)
