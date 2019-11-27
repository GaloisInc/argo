from abc import ABCMeta, abstractmethod
from dataclasses import dataclass, field
from typing import List, Optional, Set, Union

from . import SAWConnection
from argo.connection import ServerConnection
from . import llvm

designated_connection = None # type: Optional[SAWConnection]

used_server_names = set([]) # type: Set[str]

def fresh_server_name(hint : Optional[str] = None) -> str:
    if hint is None: hint = 'x'
    name = llvm.uniquify(hint, used_server_names)
    used_server_names.add(name)
    return name

def get_designated_connection() -> SAWConnection:
    global designated_connection
    if designated_connection is None:
        raise ValueError("There is not yet a designated connection.")
    else:
        return designated_connection

def connect(command_or_connection : Union[str, ServerConnection]) -> None:
    global designated_connection
    if designated_connection is None:
        designated_connection = SAWConnection(command_or_connection)
    else:
        raise ValueError("There is already a designated connection.")

def cryptol_load_file(filename : str) -> None:
    get_designated_connection().cryptol_load_file(filename)
    return None

@dataclass
class LLVMModule:
    bitcode_file : str
    server_name : str

def llvm_load_module(bitcode_file : str) -> LLVMModule:
    name = fresh_server_name(bitcode_file)
    get_designated_connection().llvm_load_module(name, bitcode_file).result()
    return LLVMModule(bitcode_file, name)

class VerificationResult(metaclass=ABCMeta):
    server_name : str
    contract : llvm.Contract

@dataclass
class VerificationSucceeded(VerificationResult):
    server_name : str
    contract : llvm.Contract

@dataclass
class Tactic:
    name : str

abc = Tactic('abc')

def llvm_verify(module : LLVMModule,
                function : str,
                contract : llvm.Contract,
                lemmas : Optional[List[VerificationResult]] = None,
                check_sat : bool = False,
                tactic : Optional[Tactic] = None,
                lemma_name_hint : Optional[str] = None) -> VerificationResult:

    if lemmas is None: lemmas = []
    if tactic is None: tactic = abc

    lemma_name_hint = contract.__class__.__name__ + "_" + function
    name = llvm.uniquify(lemma_name_hint, used_server_names)
    used_server_names.add(name)

    get_designated_connection().llvm_verify(module.server_name,
                                            function,
                                            [l.server_name for l in lemmas],
                                            check_sat,
                                            contract.contract_json(),
                                            tactic.name,
                                            name)
    return VerificationSucceeded(server_name=name, contract=contract)
