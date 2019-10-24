import os
import os.path
import saw

dir_path = os.path.dirname(os.path.realpath(__file__))

c = saw.connect("cabal new-exec --verbose=0 saw-remote-api -- --dynamic4")

seven_bc = os.path.join(dir_path, 'seven.bc')

c.llvm_load_module('m', seven_bc).result()

c.llvm_start_setup('setup').result()
c.llvm_return({"setup value": "Cryptol", "expression": "7 : [32]"}).result()
c.llvm_finish_setup().result()

print(c.llvm_verify('m', 'seven', [], False, 'setup', 'abc', 'ok').result())
