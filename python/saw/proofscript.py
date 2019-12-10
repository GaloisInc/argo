def proof_script(tactics):
  return { "tactics": tactics }

def use_prover(prover):
  return { "tactic": "use prover", "prover": prover }

abc = use_prover({ "name": "abc" })
rme = use_prover({ "name": "rme" })

def cvc4(unints):
  return use_prover({ "name": "cvc4", "uninterpreted functions": unints })

def yices(unints):
  return use_prover({ "name": "yices", "uninterpreted functions": unints })

def z3(unints):
  return use_prover({ "name": "z3", "uninterpreted functions": unints })
