from abc import ABCMeta, abstractmethod
from dataclasses import dataclass, field
from typing import List, Optional, Set, Union, Dict, Tuple, Any
import webbrowser
import subprocess
import uuid
import os
import sys
import inspect
import posixpath
import atexit
import re

from . import SAWConnection
from argo.connection import ServerConnection
from argo.interaction import ArgoException
from . import llvm
from . import exceptions
from . import proofscript
from . import dashboard

designated_connection = None # type: Optional[SAWConnection]
designated_dashboard_path = None # type: Optional[str]
# Script-execution-global set of all results verified so far
all_verification_results = None # type: Optional[AllVerificationResults]
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

def get_designated_url() -> str:
    global designated_dashboard_path
    if designated_dashboard_path is None:
        raise ValueError("There is not yet a designated dashboard URL")
    else:
        return "http://localhost:" + str(dashboard.DEFAULT_PORT) \
            + "/" + designated_dashboard_path

def set_designated_connection(conn: SAWConnection) -> None:
    designated_connection = conn

def connect(command_or_connection : Union[str, ServerConnection],
            dashboard_path : Optional[str] = None) -> None:
    global designated_connection
    global designated_dashboard_path
    global all_verification_results
    if all_verification_results is None:
        all_verification_results = AllVerificationResults()
    else:
        raise ValueError("There is already an initialized list of verification" \
                         " results. Did you call `connect()` more than once?")
    atexit.register(qed) # call qed before shutting down
    if designated_connection is None:
        designated_connection = SAWConnection(command_or_connection)
    else:
        raise ValueError("There is already a designated connection." \
                         " Did you call `connect()` more than once?")
    if designated_dashboard_path is None:
        if dashboard_path is None:
            current_frame = inspect.currentframe()
            if current_frame is None:
                raise ValueError("Cannot automatically assign a dashboard URL" \
                                 " outside a file; use the explicit option" \
                                 " `dashboard_path = \"...\"` when calling `connect()`")
            else:
                f_back = current_frame.f_back
                filename = os.path.realpath(inspect.getfile(f_back))
                dashboard_path = \
                    re.sub(r'\.py$', '', posixpath.join(*filename.split(os.path.sep))) \
                      .replace('^/', '')
        designated_dashboard_path = dashboard_path
    else:
        raise ValueError("There is already a designated dashboard URL." \
                         " Did you call `connect()` more than once?")
    print("Dashboard:", get_designated_url(), file=sys.stderr)

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
    assumptions : List[Any]   # really, List[VerificationResult],
    contract : llvm.Contract  # but mypy doesn't allow recursive types
    _unique_id : uuid.UUID

    def __bool__(self) -> bool:
        pass

@dataclass
class VerificationSucceeded(VerificationResult):
    server_name : str
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
    __cache_dirty : bool

    def __init__(self) -> None:
        self.__results = {}
        self.__cache_dirty = True
        self.__update_dashboard__()

    def __add_result__(self, result : VerificationResult) -> None:
        self.__cache_dirty = True
        self.__results[result._unique_id] = result
        self.__update_dashboard__()

    def dot_graph(self) -> str:
        out = "digraph { \n"
        for _, result in self.__results.items():
            # Determine the node color
            if result:
                color = "blue"
                bgcolor = "lightblue"
            else:
                color = "red"
                bgcolor = "lightred"
            # Determine the node attributes
            attrs : Dict[str, str] = {
                'label': result.contract.__class__.__name__,
                'color': color,
                'bgcolor': bgcolor,
                'shape': 'box',
                'penwidth': '4',
            }
            # Render the attributes
            attr_string = ""
            for key, val in attrs.items():
                attr_string += key + " = \"" + val + "\"; "
            # Render this node line
            out += '    "' + str(result._unique_id) \
                + '" [' + attr_string.rstrip('; ') + "];\n"
            # Render each of the assumption edges
            for assumption in result.assumptions:
                out += '"' \
                    + str(result._unique_id) \
                    + '" -> "' \
                    + str(assumption._unique_id) \
                    + '"'
        out += "}"
        return out

    def svg_graph(self) -> str:
        # Generate a GraphViz DOT representation
        dot_repr = self.dot_graph()
        # Write out & render the DOT file and open it in a web browser
        svg = subprocess.check_output(["dot", "-T", "svg"],
                                      input=dot_repr,
                                      text=True)
        return svg

    def dashboard_html(self) -> str:
        return "<svg>" + self.svg_graph() + "</svg>"

    def __update_dashboard__(self) -> None:
        if designated_dashboard_path is not None:
            dashboard.serve_self_refreshing(designated_dashboard_path,
                                            os.path.basename(designated_dashboard_path),
                                            self.dashboard_html())
        else:
            ValueError("Attempted to update dashboard before it was initialized")

    def __qed__(self) -> None:
        # Iterate through all lemmata to determine if everything is okay
        # Builds a graph of all dependencies
        ok = True
        for _, result in self.__results.items():
            if not result:
                ok = False
        if not ok:
            print("Verification did not succeed.", file=sys.stderr)
            sys.exit(1)
        # sys.stdout.write(self.svg_graph())
        self.__cache_dirty = True

def llvm_verify(module : LLVMModule,
                function : str,
                contract : llvm.Contract,
                lemmas : Optional[List[VerificationResult]] = None,
                check_sat : bool = False,
                script : Optional[proofscript.ProofScript] = None,
                lemma_name_hint : Optional[str] = None) -> VerificationResult:

    if lemmas is None: lemmas = []
    if script is None: script = proofscript.ProofScript([proofscript.abc])

    lemma_name_hint = contract.__class__.__name__ + "_" + function
    name = llvm.uniquify(lemma_name_hint, used_server_names)
    used_server_names.add(name)

    result : VerificationResult
    conn = get_designated_connection()
    conn_snapshot = conn.snapshot()
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
    except exceptions.VerificationError as err:
        # roll back to snapshot because the current connection's
        # latest result is now a verification exception!
        conn = conn_snapshot
        set_designated_connection(conn)
        # assume the verification succeeded
        conn.llvm_assume(module.server_name,
                         function,
                         contract.to_json(),
                         name).result()
        result = VerificationFailed(server_name=name,
                                    assumptions=lemmas,
                                    contract=contract,
                                    exception=err)
    if all_verification_results is not None:
        all_verification_results.__add_result__(result)
    else:
        raise ValueError("Could not track verification result because" \
                         " connection is not yet initialized")
    return result

def qed() -> None:
    if all_verification_results is not None:
        all_verification_results.__qed__()
    else:
        raise ValueError("Could not finish proof because connection is not yet" \
                         "initialized")
