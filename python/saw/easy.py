from abc import ABCMeta, abstractmethod
from dataclasses import dataclass, field
from typing import List, Optional, Set, Union, Dict
import uuid

from . import SAWConnection
from argo.connection import ServerConnection
from argo.interaction import ArgoException
from . import llvm
from . import exceptions

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

def set_designated_connection(conn: SAWConnection) -> None:
    designated_connection = conn

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
    _unique_id : uuid.UUID

    def __bool__(self) -> bool:
        pass

@dataclass
class VerificationSucceeded(VerificationResult):
    server_name : str
    assumptions : List[VerificationResult]
    contract : llvm.Contract

    def __init__(self,
                 server_name : str,
                 assumptions : List[VerificationResult],
                 contract : llvm.Contract) -> None:
        self.server_name = server_name
        self.assumptions = assumptions
        self.contract = contract
        self._unique_id = uuid.uuid4()

    def __bool__(self) -> bool:
        return True

@dataclass
class VerificationFailed(VerificationResult):
    server_name : str
    assumptions : List[VerificationResult]
    contract : llvm.Contract
    exception : Exception

    def __init__(self,
                 server_name : str,
                 assumptions : List[VerificationResult],
                 contract : llvm.Contract,
                 exception : Exception) -> None:
        self.server_name = server_name
        self.assumptions = assumptions
        self.contract = contract
        self.exception = exception
        self._unique_id = uuid.uuid4()

    def __bool__(self) -> bool:
        return False

class AllVerificationResults:
    __results : Dict[uuid.UUID, VerificationResult]
    __qed_called : bool = False

    def __init__(self) -> None:
        self.__results = {}
        self.__qed_called = True

    def __add_result__(self, result : VerificationResult) -> None:
        self.__qed_called = False
        self.__results[result._unique_id] = result

    def __qed__(self) -> None:
        ok = True
        for _, result in self.__results.items():
            if not result:
                ok = False
        assert ok, verification_results.__results.__repr__()
        self.__qed_called = True

# Script-execution-global set of all results verified so far
verification_results = AllVerificationResults()

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

    result : VerificationResult
    contract_json = contract.contract_json()
    conn = get_designated_connection()
    conn_snapshot = conn.snapshot()
    try:
        conn.llvm_verify(module.server_name,
                         function,
                         [l.server_name for l in lemmas],
                         check_sat,
                         contract_json,
                         tactic.name,
                         name).result()
        result = VerificationSucceeded(server_name=name,
                                       assumptions=lemmas,
                                       contract=contract)
    except exceptions.VerificationError as err:
        # roll back to snapshot because the current connection's
        # latest result is now a verification exception!
        conn = conn_snapshot
        set_designated_connection(conn)
        # assume the verification succeeded
        conn.llvm_assume(module.server_name,
                            function,
                            contract_json,
                            name).result()
        result = VerificationFailed(server_name=name,
                                    assumptions=lemmas,
                                    contract=contract,
                                    exception=err)
    verification_results.__add_result__(result)
    return result

def qed() -> None:
    verification_results.__qed__()
