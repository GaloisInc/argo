import argo.interaction
import inspect
from argo.interaction import HasProtocolState

from typing import Any, List, Optional, Tuple, Dict

class CryptolLoadFile(argo.interaction.Command):
    def __init__(self, connection : HasProtocolState, filename : str) -> None:
        super(CryptolLoadFile, self).__init__(
            'SAW/Cryptol/load file',
            {'file': filename},
            connection
        )

    def process_result(self, _res : Any) -> Any:
        return None

class CryptolLoadModule(argo.interaction.Command):
    def __init__(self, connection : HasProtocolState, module_name : str) -> None:
        super(CryptolLoadModule, self).__init__(
            'SAW/Cryptol/load module',
            {'module': module_name},
            connection
        )

    def process_result(self, _res : Any) -> Any:
        return None

class LLVMLoadModule(argo.interaction.Command):
    def __init__(self, connection : HasProtocolState,
                 name : str,
                 bitcode_file : str) -> None:
        super(LLVMLoadModule, self).__init__(
            'SAW/LLVM/load module',
            {'name': name, 'bitcode file': bitcode_file},
            connection
        )

    def process_result(self, _res : Any) -> Any:
        return None

class LLVMAssume(argo.interaction.Command):
    def __init__(
            self,
            connection : HasProtocolState,
            module : str,
            function : str,
            setup : Any,
            lemma_name : str) -> None:
        params = {'module': module,
                  'function': function,
                  'contract': setup,
                  'lemma name': lemma_name}
        super(LLVMAssume, self).__init__('SAW/LLVM/assume', params, connection)

    def process_result(self, _res : Any) -> Any:
        return None

class LLVMVerify(argo.interaction.Command):
    def __init__(
            self,
            connection : HasProtocolState,
            module : str,
            function : str,
            lemmas : List[str],
            check_sat : bool,
            setup : Any,
            tactic : str,
            lemma_name : str,
            verifications : Optional[List[Tuple[str, int, Dict[str, str], bool]]] = None,
            location : Optional[Tuple[str, int]] = None) -> None:
        params = {'module': module,
                  'function': function,
                  'lemmas': lemmas,
                  'check sat': check_sat,
                  'contract': setup,
                  'tactic': tactic,
                  'lemma name': lemma_name}
        self.params = params
        # Determine the location of this verification call: either where the
        # command was created, or the passed-in location
        self.filename : Optional[str] = None
        self.line     : Optional[int] = None
        if location is None:
            calling_frame = inspect.currentframe()
            if calling_frame is not None:
                self.filename = calling_frame.f_back.f_code.co_filename
                self.line     = calling_frame.f_back.f_lineno
        else:
            self.filename = location[0]
            self.line     = location[1]
        self.verifications = verifications
        super(LLVMVerify, self).__init__('SAW/LLVM/verify', params, connection)

    def process_result(self, _res : Any) -> Any:
        if self.verifications is not None:
            if self.filename is not None and self.line is not None:
                self.verifications.append((self.filename, self.line, self.params, True))
        return True

    def process_error(self, code : int, data : Any, msg : str) -> Any:
        if (self.verifications is not None) and ((code >= 10300) and (code < 20000)):
            if self.filename is not None and self.line is not None:
                self.verifications.append((self.filename, self.line, self.params, False))
            return False
        else:
            return None
