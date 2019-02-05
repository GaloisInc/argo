import SAW

saw = SAW.connect()

swap_mod = saw.llvm.load_module("swap-correct.bc")

model = swap_mod.extract("swap_correct")

thm = saw.abc.prove_print(saw.cryptol("\ x y -> %{model} x y != 0") % {'model': model})


class SwapSpec (SAW.LLVMSpec):
  def spec(self):
    x = self.fresh_var("x", saw.llvm.int_t(32))
    y = self.fresh_var("y", saw.llvm.int_t(32))
    xp = self.alloc(saw.llvm.int_t(32))
    yp = self.alloc(saw.llvm.int_t(32))
    self.points_to(xp, x)
    self.points_to(yp, y)

    self.call(xp, yp)

    self.points_to(xp, y)
    self.points_to(yp, x)


class SwapSpec2 (SAW.LLVMSpec):
  def pointer_to_fresh(self, name, ty):
      x = self.fresh_var(name, ty)
      xp = self.alloc(ty)
      self.points_to(xp, x)

      return (x, xp)

  def spec(self):
      x, xp = self.pointer_to_fresh("x", saw.llvm.int_t(32))
      y, yp = self.pointer_to_fresh("y", saw.llvm.int_t(32))

      self.call(xp, yp)

      self.points_to(xp, y)
      self.points_to(yp, x)


saw.crucible.llvm_verify(swap_mod, "swap_xor", SwapSpec2(), extra_specs=[], path_sat=True, prover=saw.abc)

