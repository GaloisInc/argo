import SAW


class SpecUtils:
    def pointer_to_fresh(self, name, ty):
        x = self.fresh_var(name, ty)
        xp = self.alloc(ty)
        self.points_to(xp, x)

        return (x, xp)

saw = SAW.connect()

saw.import_cryptol("Salsa20.cry")

salsa20_mod = saw.llvm.load_module("salsa20.bc")

i8 = saw.llvm.int_t(8);

class Salsa20Encrypt32Spec(SAW.LLVMSpec, SpecUtils):
    def __init__(self, width):
        self.width = width

    def i8_array(self, size):
        return saw.llvm.array_t(size, i8)

    def spec(self):
        key, pkey = self.pointer_to_fresh("key", self.i8_array(32))
        v, pv = self.pointer_to_fresh("key", self.i8_array(32))
        m, pm = self.pointer_to_fresh("buf", self.i8_array(self.width))
        si = self.cryptol("0 : [32]")
        buflen = self.cryptol("`%{n} : 32") % {'n' : self.width}

        self.call(pkey, pv, si, pm, buflen)

        self.points_to(pm, self.cryptol("Salsa20_encrypt (%key, %v, %m)" % {'key' : key, 'v':  v, 'm': m}))
        self.returns(self.cryptol("0 : [32]"))

# TODO add time call from Python lib
for size in [63, 64, 65]:
    saw.crucible.llvm_verify(salsa20_mod, "s20_crypt32", Salsa20Encrypt32Spec(size))

print("Done!")
