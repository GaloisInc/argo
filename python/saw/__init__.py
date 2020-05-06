from abc import ABCMeta, abstractmethod
from dataclasses import dataclass, field
from typing import List, Optional, Set, Union, Dict, Tuple, Any, Callable
import webbrowser
import subprocess
import uuid
import os
import sys
import inspect
import posixpath
import atexit
import re

from . import connection
from argo.connection import ServerConnection
from argo.interaction import ArgoException
from . import llvm
from . import exceptions
from . import proofscript

__designated_connection = None  # type: Optional[connection.SAWConnection]
__designated_views = []         # type: List[View]
__global_success = True         # type: bool

# Script-execution-global set of all results verified so far
__used_server_names = set([])      # type: Set[str]


def __fresh_server_name(hint: Optional[str] = None) -> str:
    if hint is None:
        hint = 'x'
    name = llvm.uniquify(hint, __used_server_names)
    __used_server_names.add(name)
    return name


def __get_designated_connection() -> connection.SAWConnection:
    global __designated_connection
    if __designated_connection is None:
        raise ValueError("There is not yet a designated connection.")
    else:
        return __designated_connection


def __set_designated_connection(conn: connection.SAWConnection) -> None:
    global __designated_connection
    __designated_connection = conn


class VerificationResult(metaclass=ABCMeta):
    server_name: str
    assumptions: List[Any]   # really, List[VerificationResult],
    contract: llvm.Contract  # but mypy doesn't allow recursive types
    _unique_id: uuid.UUID

    def __bool__(self) -> bool:
        pass


@dataclass
class VerificationSucceeded(VerificationResult):
    def __init__(self,
                 server_name: str,
                 assumptions: List[VerificationResult],
                 contract: llvm.Contract) -> None:
        self.server_name = server_name
        self.assumptions = assumptions
        self.contract = contract
        self._unique_id = uuid.uuid4()

    def __bool__(self) -> bool:
        return True


@dataclass
class VerificationFailed(VerificationResult):
    exception: exceptions.VerificationError

    def __init__(self,
                 server_name: str,
                 assumptions: List[VerificationResult],
                 contract: llvm.Contract,
                 exception: exceptions.VerificationError) -> None:
        self.server_name = server_name
        self.assumptions = assumptions
        self.contract = contract
        self.exception = exception
        self._unique_id = uuid.uuid4()

    def __bool__(self) -> bool:
        return False


@dataclass
class AssumptionFailed(VerificationFailed):
    def __init__(self,
                 server_name: str,
                 assumptions: List[VerificationResult],
                 contract: llvm.Contract,
                 exception: exceptions.VerificationError) -> None:
        super().__init__(server_name,
                         assumptions,
                         contract,
                         exception)


def connect(command_or_connection: Union[str, ServerConnection],
            *, persist: bool = False) -> None:
    global __designated_connection

    # Set the designated connection by starting a server process
    if __designated_connection is None:
        __designated_connection = \
            connection.SAWConnection(command_or_connection, persist=persist)
    else:
        raise ValueError("There is already a designated connection."
                         " Did you call `connect()` more than once?")

    # After the script quits, print the server PID
    if persist:
        def print_if_still_running():
            try:
                pid = __designated_connection.pid()
                if __designated_connection.running():
                    message = f"Created persistent server process: PID {pid}"
                    print(message)
            except ProcessLookupError:
                pass
        atexit.register(print_if_still_running)


class View:
    """An instance of View describes how to (potentially interactively) view
       or log the results of a verification script, in real-time."""

    def on_failure(self, failure: VerificationFailed) -> None:
        """When a verification attempt fails, do this."""
        pass

    def on_success(self, success: VerificationSucceeded) -> None:
        """When a verification attempt succeeds, do this."""
        pass

    def on_finish_success(self) -> None:
        """After all verifications are finished successfully, do this."""
        pass

    def on_finish_failure(self) -> None:
        """After all verifications are finished but with some failures,
           do this."""
        pass


class LogResults(View):
    """A view on the verification results that logs failures and successes in a
       human-readable text format to stdout, or a given file handle."""

    def __init__(self, file=sys.stdout):
        self.file = file
        self.all_ok = True

    def __result_attributes(result):
        lineno = result.contract.definition_lineno()
        if lineno is None:
            lineno = "<unknown line>"
        filename = result.contract.definition_filename()
        if filename is None:
            filename = "<unknown file>"
        lemma_name = result.contract.lemma_name()
        return (filename, lineno, lemma_name)

    def on_failure(self, result) -> None:
        filename, lineno, lemma_name = LogResults.__result_attributes(result)
        print(f"Failed to verify: {lemma_name}"
              f" (defined at {filename}:{lineno}): {result.exception}",
              file=self.file)

    def on_success(self, result) -> None:
        filename, lineno, lemma_name = LogResults.__result_attributes(result)
        print(f"Verified: {lemma_name}"
              f" (defined at {filename}:{lineno})",
              file=self.file)

    def on_finish_success(self) -> None:
        print("All verified!", file=self.file)

    def on_finish_failure(self) -> None:
        print("Some lemmas failed to verify.", file=self.file)


def view(v: View) -> None:
    """Add a view to the global list of views. Future verification results will
       be handed to this view, and its on_finish() handler will be called at
       the end of the script."""
    global __designated_views
    __designated_views.append(v)


def cryptol_load_file(filename: str) -> None:
    __get_designated_connection().cryptol_load_file(filename)
    return None


@dataclass
class LLVMModule:
    bitcode_file: str
    server_name: str


def llvm_load_module(bitcode_file: str) -> LLVMModule:
    name = __fresh_server_name(bitcode_file)
    __get_designated_connection().llvm_load_module(name, bitcode_file).result()
    return LLVMModule(bitcode_file, name)


def llvm_verify(module: LLVMModule,
                function: str,
                contract: llvm.Contract,
                lemmas: Optional[List[VerificationResult]] = None,
                check_sat: bool = False,
                script: Optional[proofscript.ProofScript] = None,
                lemma_name_hint: Optional[str] = None) -> VerificationResult:

    if lemmas is None:
        lemmas = []
    if script is None:
        script = proofscript.ProofScript([proofscript.abc])

    lemma_name_hint = contract.__class__.__name__ + "_" + function
    name = llvm.uniquify(lemma_name_hint, __used_server_names)
    __used_server_names.add(name)

    result: VerificationResult
    conn = __get_designated_connection()
    conn_snapshot = conn.snapshot()
    abort_proof = False
    try:
        conn.llvm_verify(module.server_name,
                         function,
                         [l.server_name for l in lemmas],
                         check_sat,
                         contract.to_json(),
                         script.to_json(),
                         name).result()
        result = VerificationSucceeded(server_name=name,
                                       assumptions=lemmas,
                                       contract=contract)
    # If the verification did not succeed...
    except exceptions.VerificationError as err:
        # roll back to snapshot because the current connection's
        # latest result is now a verification exception!
        __set_designated_connection(conn_snapshot)
        conn = __get_designated_connection()
        # Assume the verification succeeded
        try:
            conn.llvm_assume(module.server_name,
                             function,
                             contract.to_json(),
                             name).result()
            result = VerificationFailed(server_name=name,
                                        assumptions=lemmas,
                                        contract=contract,
                                        exception=err)
        # If something stopped us from even **assuming**...
        except exceptions.VerificationError as err:
            __set_designated_connection(conn_snapshot)
            result = AssumptionFailed(server_name=name,
                                      assumptions=lemmas,
                                      contract=contract,
                                      exception=err)
            abort_proof = True

    # Log or otherwise process the verification result
    global __designated_views
    for view in __designated_views:
        if result:
            view.on_success(result)
        else:
            view.on_failure(result)

    # Abort the proof if we failed to assume a failed verification, otherwise
    # return the result of the verification
    if abort_proof:
        sys.exit(1)

    # Note when any failure occurs
    global __global_success
    __global_success = __global_success and result

    return result


@atexit.register
def script_exit():
    global __designated_views
    for view in __designated_views:
        if __global_success:
            view.on_finish_success()
        else:
            view.on_finish_failure()
