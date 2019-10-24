import argo.interaction
from argo.interaction import HasProtocolState

from typing import Any, List

class CryptolStartSetup(argo.interaction.Command):
    def __init__(self, connection : HasProtocolState, name : str) -> None:
        super(CryptolStartSetup, self).__init__(
            'SAW/Cryptol/start setup',
            {'name': name},
            connection
        )

    def process_result(self, _res : Any) -> Any:
        return None

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

class CryptolFinishSetup(argo.interaction.Command):
    def __init__(self, connection : HasProtocolState) -> None:
        super(CryptolFinishSetup, self).__init__(
            'SAW/Cryptol/finish setup',
            {},
            connection
        )

    def process_result(self, _res : Any) -> Any:
        return None

class LLVMStartSetup(argo.interaction.Command):
    def __init__(self, connection : HasProtocolState, name : str) -> None:
        super(LLVMStartSetup, self).__init__(
            'SAW/LLVM/start setup',
            {'name': name},
            connection
        )

    def process_result(self, _res : Any) -> Any:
        return None

class LLVMFinishSetup(argo.interaction.Command):
    def __init__(self, connection : HasProtocolState) -> None:
        super(LLVMFinishSetup, self).__init__('SAW/LLVM/finish setup', {}, connection)

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

class LLVMReturn(argo.interaction.Command):
    def __init__(self, connection : HasProtocolState, return_value : Any) -> None:
        super(LLVMReturn, self).__init__('SAW/LLVM/return', {'value': return_value}, connection)

    def process_result(self, _res : Any) -> Any:
        return None

class LLVMFresh(argo.interaction.Command):
    def __init__(self, connection : HasProtocolState, return_value : Any) -> None:
        super(LLVMFresh, self).__init__('SAW/LLVM/fresh', return_value, connection)

    def process_result(self, _res : Any) -> Any:
        return None

class LLVMPointsTo(argo.interaction.Command):
    def __init__(self, connection : HasProtocolState, pointer : Any, target : Any) -> None:
        super(LLVMPointsTo, self).__init__('SAW/LLVM/points to', {'pointer': pointer, 'target': target})

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
            setup : str,
            tactic : str,
            lemma_name : str) -> None:
        params = {'module': module,
                  'function': function,
                  'lemmas': lemmas,
                  'check sat': check_sat,
                  'setup': setup,
                  'tactic': tactic,
                  'lemma name': lemma_name}
        super(LLVMVerify, self).__init__('SAW/LLVM/verify', params, connection)

    def process_result(self, _res : Any) -> Any:
        return None
