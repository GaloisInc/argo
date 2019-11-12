import os
import os.path
import saw

dir_path = os.path.dirname(os.path.realpath(__file__))

c = saw.connect("cabal new-exec --verbose=0 saw-remote-api -- --dynamic4")

swap_bc = os.path.join(dir_path, 'swap.bc')

c.llvm_load_module('m', swap_bc).result()

uint32_t = {"type": "primitive type", "primitive": "integer", "size": 32}

# ServerNames
xp_name = {"name": "xp"}
yp_name = {"name": "yp"}

# SetupVals
xp = {"setup value": "saved", "name": "xp"}
yp = {"setup value": "saved", "name": "yp"}
x = {"setup value": "Cryptol", "expression": "x" }
y = {"setup value": "Cryptol", "expression": "x" }

contract = {
    "pre vars": [
        ["x", "x", uint32_t],
        ["y", "y", uint32_t]
    ],
    "pre conds": [],
    "pre allocated": [
        ["xp", uint32_t],
        ["yp", uint32_t]
    ],
    "pre points tos": [[xp, x], [yp, y]],
    "argument vals": [xp, yp],
    "post vars": [],
    "post conds": [],
    "post allocated": [],
    "post points tos": [[xp, y], [yp, x]],
    "return val": None
}

print(c.llvm_verify('m', 'swap', [], False, contract, 'abc', 'ok').result())
