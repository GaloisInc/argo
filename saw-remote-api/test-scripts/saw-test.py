print("Starting")
import os
import os.path
import saw

print("Imported")

dir_path = os.path.dirname(os.path.realpath(__file__))

print("This is " + dir_path)

with saw.connect("cabal new-exec --verbose=0 saw-remote-api -- --dynamic4") as c:

    cry_file = os.path.join(dir_path, 'Foo.cry')
    c.cryptol_load_file(cry_file)


    null_bc = os.path.join(dir_path, 'null.bc')

    c.llvm_load_module('m', null_bc).result()

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
        "return val": "null"
    }

    print(c.llvm_verify('m', 'always_null', [], False, contract, 'abc', 'ok').result())
