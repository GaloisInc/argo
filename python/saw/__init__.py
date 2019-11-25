from __future__ import annotations

import inspect
import argo
import argo.connection as ac
import argo.interaction
from saw.commands import *

from typing import Optional


def connect(command : str,
            delay_verification_exceptions : Optional[bool] = False) -> SAWConnection:
    proc = ac.ServerProcess(command)
    conn = ac.ServerConnection(proc)
    return SAWConnection(conn, delay_verification_exceptions)


class SAWConnection:
    """A representation of a current user state in a session with SAW."""

    most_recent_result : Optional[argo.interaction.Interaction]

    def __init__(self,
                 server_connection : ac.ServerConnection,
                 delay_verification_exceptions : Optional[bool] = False) -> None:
        self.most_recent_result = None
        self.server_connection = server_connection
        self._verifications : Optional[List[Tuple[str, int, Dict[str, str], bool]]] = None
        if delay_verification_exceptions:
            self._verifications = []

    def snapshot(self) -> SAWConnection:
        """Return a ``SAWConnection`` that has the same process and state as
        the current connection. The new connection's state will be
        independent of the current state.
        """
        copy = SAWConnection(self.server_connection)
        copy.most_recent_result = self.most_recent_result
        if self._verifications is not None:
            copy._verifications = self._verifications[:] # shallow copy of list spine
        return copy

    def summary(self) ->  Optional[List[Tuple[str, int, Dict[str, str], bool]]]:
        """Return a summary of the verification results from this session so far, in
        the form of a list of (optional filename, optional line number, method
        parameters, verification result) tuples
        """
        if self._verifications is not None:
            return self._verifications[:]
        else:
            return None

    def assert_all_verifications(self) -> None:
        """Assert that all verifications so far have been correct; if not, throw an
        AssertionError containing the human-readable summary of the verifications"""
        readable_summary = self.readable_summary()
        if readable_summary is not None:
            (ok, summary) = readable_summary
            if not ok:
                raise AssertionError(summary)

    def readable_summary(self) -> Optional[Tuple[bool, str]]:
        """Return a human-readable summary of the verification results from this
        session so far.
        """
        if self._verifications is None:
            return None
        ok = True
        output = ""
        for (filename, line, params, result) in self._verifications:
            ok = ok and result
            if result:
                mark = '\033[1;32m✔\033[1;0m'
            else:
                mark = '\033[1;31m✘\033[1;0m'
            output_line = mark + " " + filename + ":" + str(line) + ": '" + params['lemma name'] + "'"
            output += output_line + "\n"
        return (ok, output)

    def protocol_state(self) -> Any:
        if self.most_recent_result is None:
            return []
        else:
            return self.most_recent_result.state()

    # Protocol messages
    def cryptol_load_file(self, filename : str) -> argo.interaction.Command:
        self.most_recent_result = CryptolLoadFile(self, filename)
        return self.most_recent_result

    def llvm_load_module(self, name : str, bitcode_file : str)  -> argo.interaction.Command:
        self.most_recent_result = LLVMLoadModule(self, name, bitcode_file)
        return self.most_recent_result

    def llvm_verify(self,
                    module : str,
                    function : str,
                    lemmas : List[str],
                    check_sat : bool,
                    contract : Any,
                    tactic : str,
                    lemma_name : str) -> argo.interaction.Command:
        # Determine the location of this verification call: either where the
        # command was created, or the passed-in location
        filename : Optional[str] = None
        line     : Optional[int] = None
        location : Optional[Tuple[str, int]] = None
        calling_frame = inspect.currentframe()
        if calling_frame is not None:
            filename = calling_frame.f_back.f_code.co_filename
            line     = calling_frame.f_back.f_lineno
            location = (filename, line)

        self.most_recent_result = \
            LLVMVerify(self, module, function, lemmas, check_sat, contract, tactic, lemma_name, self._verifications, location)
        return self.most_recent_result

    def llvm_assume(self,
                    module : str,
                    function : str,
                    contract : Any,
                    lemma_name : str) -> argo.interaction.Command:
        self.most_recent_result = LLVMAssume(self, module, function, contract, lemma_name)
        return self.most_recent_result
