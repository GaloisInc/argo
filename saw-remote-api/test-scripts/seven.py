import os
import os.path
import saw

dir_path = os.path.dirname(os.path.realpath(__file__))

c = saw.connect("cabal new-exec --verbose=0 saw-remote-api -- --dynamic4")

seven_bc = os.path.join(dir_path, 'seven.bc')

c.llvm_load_module('m', seven_bc).result()

contract = {
    "pre vars": [],
    "pre conds": [],
    "pre allocated": [],
    "pre points tos": [],
    "argument vals": [],
    "post vars": [],
    "post conds": [],
    "post allocated": [],
    "post points tos": [],
    "return val": {"setup value": "Cryptol", "expression": "7 : [32]"}
}

print(c.llvm_verify('m', 'seven', [], False, contract, 'abc', 'ok').result())
