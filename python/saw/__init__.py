from abc import ABCMeta, abstractmethod
from dataclasses import dataclass, field
from typing import List, Optional, Set, Union, Dict, Tuple, Any, Callable, IO
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

    @abstractmethod
    def is_success(self) -> bool: ...


@dataclass
class VerificationSucceeded(VerificationResult):
    def __init__(self,
                 server_name: str,
                 assumptions: List[VerificationResult],
                 contract: llvm.Contract,
                 stdout: str,
                 stderr: str) -> None:
        self.server_name = server_name
        self.assumptions = assumptions
        self.contract = contract
        self._unique_id = uuid.uuid4()
        self.stdout = stdout
        self.stderr = stderr

    def is_success(self) -> bool:
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

    def is_success(self) -> bool:
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
        def print_if_still_running() -> None:
            if __designated_connection is not None:
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

    def on_python_exception(self, exception: Exception) -> None:
        """When some Python exception occurs, do this."""
        pass

    def on_finish_success(self) -> None:
        """After all verifications are finished successfully, do this."""
        pass

    def on_finish_failure(self) -> None:
        """After all verifications are finished but with some failures,
           do this."""
        pass

    def on_abort(self) -> None:
        """If the proof is aborted due to inability to assume a lemma, do
        this."""
        pass


class DebugLog(View):
    """A view on the verification results that logs the stdout/stderr of the
    method to stdout/stderr, or specified file handles."""

    def __init__(self, *, out: IO[str] = sys.stdout, err: IO[str] = sys.stderr) -> None:
        self.out = out
        self.err = err

    def on_failure(self, failure: VerificationFailed) -> None:
        if self.out is not None:
            print(failure.exception.stdout, file=self.out, end='')
        if self.err is not None:
            print(failure.exception.stderr, file=self.err, end='')

    def on_success(self, success: VerificationSucceeded) -> None:
        if self.out is not None:
            print(success.stdout, file=self.out, end='')
        if self.err is not None:
            print(success.stderr, file=self.err, end='')

        
class LogResults(View):
    """A view on the verification results that logs failures and successes in a
       human-readable text format to stdout, or a given file handle."""

    def __init__(self, file: IO[str] = sys.stdout):
        self.file = file
        self.successes: List[VerificationSucceeded] = []
        self.failures: List[VerificationFailed] = []

    def format_failure(self, failure: VerificationFailed) -> str:
        filename, lineno, lemma_name = self.__result_attributes(failure)
        return (f"⚠️  Failed to verify: {lemma_name}"
                f" (defined at {filename}:{lineno}):\n{failure.exception}")

    def format_success(self, success: VerificationSucceeded) -> str:
        filename, lineno, lemma_name = self.__result_attributes(success)
        return (f"✅  Verified: {lemma_name}"
                f" (defined at {filename}:{lineno})")

    @staticmethod
    def __result_attributes(result: VerificationResult) -> Tuple[str, Union[int, str], str]:
        lineno: Optional[Union[int, str]] = result.contract.definition_lineno()
        if lineno is None:
            lineno = "<unknown line>"
        filename = result.contract.definition_filename()
        if filename is None:
            filename = "<unknown file>"
        lemma_name = result.contract.lemma_name()
        return (filename, lineno, lemma_name)

    def on_failure(self, result: VerificationFailed) -> None:
        self.failures.append(result)
        print(self.format_failure(result), file=self.file)

    def on_success(self, result: VerificationSucceeded) -> None:
        self.successes.append(result)
        print(self.format_success(result), file=self.file)

    def on_finish_success(self) -> None:
        print("✅  All verified!", file=self.file)

    def on_finish_failure(self) -> None:
        print("🛑  Some lemmas failed to verify.", file=self.file)

    def on_abort(self) -> None:
        print("🛑  Aborting proof script.", file=self.file)

        
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

    global __global_success
    global __designated_views
    
    try:
        res = conn.llvm_verify(module.server_name,
                               function,
                               [l.server_name for l in lemmas],
                               check_sat,
                               contract.to_json(),
                               script.to_json(),
                               name)
        stdout = res.stdout()
        stderr = res.stderr()
        result = VerificationSucceeded(server_name=name,
                                       assumptions=lemmas,
                                       contract=contract,
                                       stdout=stdout,
                                       stderr=stderr)
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
    # If something else went wrong...
    except Exception as err:
        __global_success = False
        for view in __designated_views:
            view.on_python_exception(err)
        raise err from None

    # Log or otherwise process the verification result
    for view in __designated_views:
        if isinstance(result, VerificationSucceeded):
            view.on_success(result)
        elif isinstance(result, VerificationFailed):
            view.on_failure(result)

    # Note when any failure occurs
    __global_success = __global_success and result.is_success()

    # Abort the proof if we failed to assume a failed verification, otherwise
    # return the result of the verification
    if isinstance(result, AssumptionFailed):
        raise result.exception from None

    return result


@atexit.register
def script_exit() -> None:
    global __designated_views
    for view in __designated_views:
        if __global_success:
            view.on_finish_success()
        else:
            view.on_finish_failure()
