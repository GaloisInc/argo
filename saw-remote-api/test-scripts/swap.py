import os
import os.path
import saw

dir_path = os.path.dirname(os.path.realpath(__file__))

c = saw.connect("cabal new-exec --verbose=0 saw-remote-api -- --dynamic4")

swap_bc = os.path.join(dir_path, 'swap.bc')

c.llvm_load_module('m', swap_bc).result()

uint32_t = {"type": "primitive type", "primitive": "integer", "size": 32}

c.llvm_start_setup('setup').result()
print(c.llvm_fresh({"name": "x", "type": uint32_t}).result())
print(c.llvm_fresh({"name": "y", "type": uint32_t}).result())
c.llvm_points_to("x", "y").result()
c.llvm_finish_setup().result()

print(c.llvm_verify('m', 'swap', [], False, 'setup', 'abc', 'ok').result())
