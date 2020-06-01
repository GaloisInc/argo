import os
from pathlib import Path

import argo.connection as argo
import cryptol
from cryptol import CryptolConnection, CryptolContext, cry

dir_path = Path(os.path.dirname(os.path.realpath(__file__)))

cryptol_path = dir_path.joinpath('test-data')

c = argo.ServerConnection(
      cryptol.CryptolProcess(
          "cabal v2-exec cryptol-remote-api  --verbose=0",
          cryptol_path=cryptol_path))

# Regression tests on nested sequences

id_1 = c.send_message("load module", {"module name": "M", "state": []})
reply_1 = c.wait_for_reply_to(id_1)
print(reply_1)
assert('result' in reply_1)
assert('state' in reply_1['result'])
assert('answer' in reply_1['result'])
state_1 = reply_1['result']['state']

id_2 = c.send_message("evaluate expression", {"expression": {"expression":"call","function":"f","arguments":[{"expression":"bits","encoding":"hex","data":"ff","width":8}]}, "state": state_1})
reply_2 = c.wait_for_reply_to(id_2)
assert('result' in reply_2)
assert('answer' in reply_2['result'])
assert('value' in reply_2['result']['answer'])
assert(reply_2['result']['answer']['value'] ==
       {'data': [{'data': 'ff', 'width': 8, 'expression': 'bits', 'encoding': 'hex'},
                 {'data': 'ff', 'width': 8, 'expression': 'bits', 'encoding': 'hex'}],
        'expression': 'sequence'})

id_3 = c.send_message("evaluate expression", {"expression": {"expression":"call","function":"g","arguments":[{"expression":"bits","encoding":"hex","data":"ff","width":8}]}, "state": state_1})
reply_3 = c.wait_for_reply_to(id_3)
assert('result' in reply_3)
assert('answer' in reply_3['result'])
assert('value' in reply_3['result']['answer'])
assert(reply_3['result']['answer']['value'] ==
       {'data': [{'data': 'ff', 'width': 8, 'expression': 'bits', 'encoding': 'hex'},
                 {'data': 'ff', 'width': 8, 'expression': 'bits', 'encoding': 'hex'}],
        'expression': 'sequence'})

id_4 = c.send_message("evaluate expression", {"expression":{"expression":"call","function":"h","arguments":[{"expression":"sequence","data":[{"expression":"bits","encoding":"hex","data":"ff","width":8},{"expression":"bits","encoding":"hex","data":"ff","width":8}]}]}, "state": state_1})
reply_4 = c.wait_for_reply_to(id_4)
assert('result' in reply_4)
assert('answer' in reply_4['result'])
assert('value' in reply_4['result']['answer'])
assert(reply_4['result']['answer']['value'] ==
       {'data': [{'data': 'ff', 'width': 8, 'expression': 'bits', 'encoding': 'hex'},
                 {'data': 'ff', 'width': 8, 'expression': 'bits', 'encoding': 'hex'}],
        'expression': 'sequence'})

a_record = {"expression": "record",
            "data": {"unit": "()",
                     "fifteen": {"expression": "bits",
                                 "encoding": "hex",
                                 "width": 4,
                                 "data": "f"}}}

id_5 = c.send_message("evaluate expression", {"state": state_1, "expression": a_record})
reply_5 = c.wait_for_reply_to(id_5)
assert('result' in reply_5)
assert('answer' in reply_5['result'])
assert('value' in reply_5['result']['answer'])
assert(reply_5['result']['answer']['value'] ==
       {'expression': 'record',
        'data': {'fifteen':
                 {'data': 'f',
                  'width': 4,
                  'expression': 'bits',
                  'encoding': 'hex'},
                 'unit':
                 {'expression': 'unit'}}})

id_6 = c.send_message("evaluate expression",
                      {"state": state_1,
                       "expression": {"expression": "let",
                                      "binders": [{"name": "theRecord", "definition": a_record}],
                                      "body": {"expression": "tuple",
                                               "data": [a_record, "theRecord"]}}})
reply_6 = c.wait_for_reply_to(id_6)
assert('result' in reply_6)
assert('answer' in reply_6['result'])
assert('value' in reply_6['result']['answer'])
assert(reply_6['result']['answer']['value'] ==
       {'expression': 'tuple',
        'data': [{'data': {'fifteen': {'data': 'f', 'width': 4, 'expression': 'bits', 'encoding': 'hex'},
                           'unit': {'expression': 'unit'}},
                  'expression': 'record'},
                 {'data': {'fifteen': {'data': 'f', 'width': 4, 'expression': 'bits', 'encoding': 'hex'},
                           'unit': {'expression': 'unit'}},
                  'expression': 'record'}]})

id_7 = c.send_message("evaluate expression",
                      {"state": state_1,
                       "expression": {"expression": "sequence",
                                      "data": [a_record, a_record]}})
reply_7 = c.wait_for_reply_to(id_7)
assert('result' in reply_7)
assert('answer' in reply_7['result'])
assert('value' in reply_7['result']['answer'])
assert(reply_7['result']['answer']['value'] ==
       {'expression': 'sequence',
        'data': [{'data': {'fifteen': {'data': 'f', 'width': 4, 'expression': 'bits', 'encoding': 'hex'},
                           'unit': {'expression': 'unit'}},
                  'expression': 'record'},
                 {'data': {'fifteen': {'data': 'f', 'width': 4, 'expression': 'bits', 'encoding': 'hex'},
                           'unit': {'expression': 'unit'}},
                  'expression': 'record'}]})

id_8 = c.send_message("evaluate expression",
                      {"state": state_1,
                       "expression": {"expression": "integer modulo",
                                      "integer": 14,
                                      "modulus": 42}})
reply_8 = c.wait_for_reply_to(id_8)
assert('result' in reply_8)
assert('answer' in reply_8['result'])
assert('value' in reply_8['result']['answer'])
print(reply_8)
assert(reply_8['result']['answer']['value'] ==
       {"expression": "integer modulo",
        "integer": 14,
        "modulus": 42})

id_9 = c.send_message("evaluate expression",
                      {"state": state_1,
                       "expression": "m `{a=60}"})
reply_9 = c.wait_for_reply_to(id_9)
print(reply_9)
assert('result' in reply_9)
assert('answer' in reply_9['result'])
assert('value' in reply_9['result']['answer'])
assert(reply_9['result']['answer']['value'] ==
       {"expression": "integer modulo",
        "integer": 42,
        "modulus": 60})
