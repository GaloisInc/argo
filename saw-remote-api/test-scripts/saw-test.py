print("Starting")
import os
import os.path
import saw

print("Imported")

dir_path = os.path.dirname(os.path.realpath(__file__))

print("This is " + dir_path)

c = saw.connect("cabal new-exec --verbose=0 saw-remote-api -- --dynamic4")

cry_file = os.path.join(dir_path, 'Foo.cry')
c.cryptol_load_file(cry_file)


null_bc = os.path.join(dir_path, 'null.bc')

c.llvm_load_module('m', null_bc).result()

c.llvm_start_setup('setup').result()
c.llvm_return('null').result()
c.llvm_finish_setup().result()

print(c.llvm_verify('m', 'always_null', [], False, 'setup', 'abc', 'ok').result())
