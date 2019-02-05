import SAW

saw = SAW.connect()

saw.import_cryptol("Salsa20.cry")

salsa20_mod = saw.llvm.load_module("salsa20.bc")


class SpecUtils(SAW.LLVMSpec):
    def pointer_to_fresh(self, name, ty):
        x = self.fresh_var(name, ty)
        xp = self.alloc(ty)
        self.points_to(xp, x)

        return (x, xp)

    def alloc_init(self, ty, val):
        p = self.alloc(ty)
        self.points_to(p, v)
        return p


class PtrUpdateSpec(SpecUtils):
    def __init__(self, n, size, ty, f):
        self.n = n
        self.size = size
        self.ty = ty
        self.f = f

    def spec(self):
        x, p = self.ptr_to_fresh(self.n, saw.llvm.array_t(self.size, self.ty))
        self.call(self.p)
        self.points_to(self.p, self.cryptol("%f %x") % {'f': self.f, 'x': self.x})


i8 = saw.llvm.int_t(8)
i32 = saw.llvm.int_t(32)

class QuarterRoundSetup(SpecUtils):
    def spec(self):
        y0, p0 = self.ptr_to_fresh("y0", i32)
        y1, p1 = self.ptr_to_fresh("y1", i32)
        y2, p2 = self.ptr_to_fresh("y2", i32)
        y3, p3 = self.ptr_to_fresh("y3", i32)

        self.call(p0, p1, p2, p3)

        zs = self.cryptol("quarterround [%{y0}, %{y1}, %{y2}, %{y3}]") %
               {'y0': y0, 'y1': y1, 'y2': y2, 'y3': t3}
        self.points_to(p0, self.cryptol("%{zs}@0") % {'zs': zs})
        self.points_to(p1, self.cryptol("%{zs}@1") % {'zs': zs})
        self.points_to(p2, self.cryptol("%{zs}@2") % {'zs': zs})
        self.points_to(p3, self.cryptol("%{zs}@3") % {'zs': zs})

row_round_spec    = PtrUpdateSpec("y",   16, i32, cryptol("rowround"))
column_round_spec = PtrUpdateSpec("x",   16, i32, cryptol("columnround"))
double_round_spec = PtrUpdateSpec("x",   16, i32, cryptol("doubleround"))
salsa20_spec      = PtrUpdateSpec("seq", 64, i8,  cryptol("Salsa20"))

class Salsa20Expansion32(SpecUtils):
    def spec(self):
        k, pk = self.pointer_to_fresh("k", saw.llvm.array_t(32, i8))
        n, pn = self.pointer_to_fresh("n", saw.llvm.array_t(16, i8))
        pks = self.alloc(saw.llvm.array_t(64, i8))

        self.call(pk, pn, pks)

        rks = self.cryptol("Salsa20_expansion`{a=2}(%, %)") % (k, n)
        self.points_to(pks, rks)

class Salsa20Encrypt32(SpecUtils):
    def __init__(self, size):
        self.size = size

    def spec(self):
        key, pkey = self.pointer_to_fresh("key", saw.llvm.array_t(32, i8))
        v, pv     = self.pointer_to_fresh("nonce", saw.llvm.array_t( 8, i8))
        m, pm     = self.pointer_to_fresh("buf", saw.llvm.array_t( n, i8))

        self.call(pkey, pv, self.cryptol("0 : [32]"), pm, self.cryptol("`% : [32]") % self.size)
        self.points_to(pm, self.cryptol("Salsa20_encrypt (%{key}, %{v}, %{m})") % {'key': key, 'v': v, 'm': m})
        self.returns(self.cryptol("0 : [32]"))

# TODO translate me
# let main : TopLevel () = do {
#     m <- llvm_load_module "salsa20.bc";
#     let verify f ovs spec = crucible_llvm_verify m f ovs true spec abc;
#     qr     <- verify "s20_quarterround" []       quarterround_setup;
#     rr     <- verify "s20_rowround"     [qr]     rowround_setup;
#     cr     <- verify "s20_columnround"  [qr]     columnround_setup;
#     dr     <- verify "s20_doubleround"  [cr,rr]  doubleround_setup;
#     s20    <- verify "s20_hash"         [dr]     salsa20_setup;
#     s20e32 <- verify "s20_expand32"     [s20]    salsa20_expansion_32;
#     s20encrypt63 <- time (verify "s20_crypt32"  [s20e32] (s20_encrypt32 63));
#     s20encrypt64 <- time (verify "s20_crypt32"  [s20e32] (s20_encrypt32 64));
#     s20encrypt65 <- time (verify "s20_crypt32"  [s20e32] (s20_encrypt32 65));
#     print "Done!";
# };
