#!/usr/bin/env python3
import os
import os.path
import saw
from saw.llvm import *

dir_path = os.path.dirname(os.path.realpath(__file__))

print("Starting server")
c = saw.connect("cabal new-exec --verbose=0 saw-remote-api -- --dynamic4")

bcname = os.path.join(dir_path, 'salsa20.bc')
cryname = os.path.join(dir_path, 'Salsa20.cry')

print("Loading Cryptol spec")
c.cryptol_load_file(cryname).result()

print("Loading LLVM module")
c.llvm_load_module('m', bcname).result()

# SetupVals
value = {"setup value": "Cryptol", "expression": "value" }
shift = {"setup value": "Cryptol", "expression": "shift" }
res = {"setup value": "Cryptol", "expression": "value <<< shift" }

y0p = {"setup value": "saved", "name" : "y0p" }
y1p = {"setup value": "saved", "name" : "y1p" }
y2p = {"setup value": "saved", "name" : "y2p" }
y3p = {"setup value": "saved", "name" : "y3p" }

y0 = {"setup value": "Cryptol", "expression" : "y0" }
y1 = {"setup value": "Cryptol", "expression" : "y1" }
y2 = {"setup value": "Cryptol", "expression" : "y2" }
y3 = {"setup value": "Cryptol", "expression" : "y3" }

y0f = {"setup value": "Cryptol", "expression" : "(quarterround [y0, y1, y2, y3]) @ 0" }
y1f = {"setup value": "Cryptol", "expression" : "(quarterround [y0, y1, y2, y3]) @ 1" }
y2f = {"setup value": "Cryptol", "expression" : "(quarterround [y0, y1, y2, y3]) @ 2" }
y3f = {"setup value": "Cryptol", "expression" : "(quarterround [y0, y1, y2, y3]) @ 3" }

yp = {"setup value": "saved", "name" : "yp" }
y = {"setup value": "Cryptol", "expression" : "y" }

rr_res = {"setup value": "Cryptol", "expression" : "rowround y" }
cr_res = {"setup value": "Cryptol", "expression" : "columnround y" }
dr_res = {"setup value": "Cryptol", "expression" : "doubleround y" }
hash_res = {"setup value": "Cryptol", "expression" : "Salsa20 y" }
expand_res = {"setup value": "Cryptol", "expression" : "Salsa20_expansion`{a=2}(k, n)" }
crypt_res = {"setup value": "Cryptol", "expression" : "Salsa20_encrypt (k, v, m)" }

rotl_contract = {
    "pre vars": [
        ["value", "value", uint32_t.to_json()],
        ["shift", "shift", uint32_t.to_json()]
    ],
    "pre conds": ["0 < shift /\\ shift < 32"],
    "pre allocated": [],
    "pre points tos": [],
    "argument vals": [value, shift],
    "post vars": [],
    "post conds": [],
    "post allocated": [],
    "post points tos": [],
    "return val": res
}

qr_contract = {
    "pre vars": [
        ["y0", "y0", uint32_t.to_json()],
        ["y1", "y1", uint32_t.to_json()],
        ["y2", "y2", uint32_t.to_json()],
        ["y3", "y3", uint32_t.to_json()]
    ],
    "pre conds": [],
    "pre allocated": [
        ["y0p", uint32_t.to_json()],
        ["y1p", uint32_t.to_json()],
        ["y2p", uint32_t.to_json()],
        ["y3p", uint32_t.to_json()]
    ],
    "pre points tos": [ [y0p, y0], [y1p, y1], [y2p, y2], [y3p, y3] ],
    "argument vals": [y0p, y1p, y2p, y3p],
    "post vars": [],
    "post conds": [],
    "post allocated": [],
    "post points tos": [ [y0p, y0f], [y1p, y1f], [y2p, y2f], [y3p, y3f] ],
    "return val": None
}

def oneptr_update_contract(ty, res):
    return {
        "pre vars": [
            ["y", "y", ty.to_json()]
        ],
        "pre conds": [],
        "pre allocated": [
            ["yp", ty.to_json()]
        ],
        "pre points tos": [ [yp, y] ],
        "argument vals": [yp],
        "post vars": [],
        "post conds": [],
        "post allocated": [],
        "post points tos": [ [yp, res] ],
        "return val": None
    }

rr_contract = oneptr_update_contract(LLVMArrayType(uint32_t, 16), rr_res)
cr_contract = oneptr_update_contract(LLVMArrayType(uint32_t, 16), cr_res)
dr_contract = oneptr_update_contract(LLVMArrayType(uint32_t, 16), dr_res)
hash_contract = oneptr_update_contract(LLVMArrayType(uint8_t, 64), hash_res)

kp = {"setup value": "saved", "name" : "kp" }
np = {"setup value": "saved", "name" : "np" }
ksp = {"setup value": "saved", "name" : "ksp" }
k = {"setup value": "Cryptol", "expression" : "k" }
n = {"setup value": "Cryptol", "expression" : "n" }
zero = {"setup value": "Cryptol", "expression" : "0 : [32]" }

expand_contract = {
    "pre vars": [
        ["k", "k", LLVMArrayType(uint8_t, 32).to_json()],
        ["n", "n", LLVMArrayType(uint8_t, 16).to_json()]
    ],
    "pre conds": [],
    "pre allocated": [
        ["kp", LLVMArrayType(uint8_t, 32).to_json()],
        ["np", LLVMArrayType(uint8_t, 16).to_json()],
        ["ksp", LLVMArrayType(uint8_t, 64).to_json()]
    ],
    "pre points tos": [ [kp, k], [np, n] ],
    "argument vals": [kp, np, ksp],
    "post vars": [],
    "post conds": [],
    "post allocated": [],
    "post points tos": [ [ksp, expand_res] ],
    "return val": None
}

vp = {"setup value": "saved", "name" : "vp" }
mp = {"setup value": "saved", "name" : "mp" }
v = {"setup value": "Cryptol", "expression" : "v" }
m = {"setup value": "Cryptol", "expression" : "m" }
def crypt_contract(size : int):
    return {
        "pre vars": [
            ["k", "k", LLVMArrayType(uint8_t, 32).to_json()],
            ["v", "v", LLVMArrayType(uint8_t, 8).to_json()],
            ["m", "m", LLVMArrayType(uint8_t, size).to_json()]
        ],
        "pre conds": [],
        "pre allocated": [
            ["kp", LLVMArrayType(uint8_t, 32).to_json()],
            ["vp", LLVMArrayType(uint8_t, 8).to_json()],
            ["mp", LLVMArrayType(uint8_t, size).to_json()]
        ],
        "pre points tos": [ [kp, k], [vp, v], [mp, m] ],
        "argument vals": [kp, vp, zero, mp, {"setup value": "Cryptol", "expression": (str(size) + " : [32]")}],
        "post vars": [],
        "post conds": [],
        "post allocated": [],
        "post points tos": [ [mp, crypt_res] ],
        "return val": zero
    }

print("Verifying rotl")
c.llvm_verify('m', 'rotl', [], False, rotl_contract, 'abc', 'rotl_ov').result()

print("Verifying s20_quarterround")
c.llvm_verify('m', 's20_quarterround', ['rotl_ov'], False, qr_contract, 'abc', 'qr_ov').result()

print("Verifying s20_rowround")
c.llvm_verify('m', 's20_rowround', ['qr_ov'], False, rr_contract, 'abc', 'rr_ov').result()

print("Verifying s20_columnround")
c.llvm_verify('m', 's20_columnround', ['rr_ov'], False, cr_contract, 'abc', 'cr_ov').result()

print("Verifying s20_doubleround")
c.llvm_verify('m', 's20_doubleround', ['cr_ov', 'rr_ov'], False, dr_contract, 'abc', 'dr_ov').result()

print("Verifying s20_hash")
c.llvm_verify('m', 's20_hash', ['dr_ov'], False, hash_contract, 'abc', 'hash_ov').result()

print("Verifying s20_expand32")
c.llvm_verify('m', 's20_expand32', ['hash_ov'], False, expand_contract, 'abc', 'expand_ov').result()

print("Verifying s20_crypt32")
c.llvm_verify('m', 's20_crypt32', ['expand_ov'], False, crypt_contract(63), 'abc', 'crypt_ov').result()

print("Done")
