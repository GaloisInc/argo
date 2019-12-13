from abc import ABC, abstractmethod
from itertools import chain
from typing import Dict, Any, List, Iterable, Type
from argo.interaction import ArgoException

class SAWException(Exception):
    data : Dict[str, Any]
    code : int

    def __init__(self, ae : ArgoException) -> None:
        super().__init__(ae.message)
        self.data = ae.data
        self.code = ae.code

    # The exception gets fields for each data field in the ArgoException
    def __getattr__(self, attr : str) -> Any:
        self.data.get(attr)

    def __dir__(self) -> Iterable[str]:
        return chain(super().__dir__(), [str(k) for k in self.data.keys()])

class UnhandledExceptionCode(Exception):
    def __init__(self) -> None:
        super().__init__()

def make_saw_exception(ae : ArgoException) -> SAWException:
    """Convert an ArgoException to its corresponding SAWException, failing with
    UnhandledExceptionCode if the code for this ArgoException is invalid.
    """
    specific_exception_class = error_code_table.get(ae.code)
    if specific_exception_class is not None:
        return specific_exception_class(ae)
    else:
        raise UnhandledExceptionCode()

# Server value errors:
class ServerValueError(SAWException): pass
class NoServerValue(ServerValueError): pass
class NotACryptolEnvironment(ServerValueError): pass
class NotAnLLVMModule(ServerValueError): pass
class NotAnLLVMSetupScript(ServerValueError): pass
class NotAnLLVMMethodSpecification(ServerValueError): pass

# Setup errors:
class SetupError(SAWException): pass
class NotSettingUpCryptol(SetupError): pass
class NotSettingUpCrucibleLLVM(SetupError): pass
class NotAtTopLevel(SetupError): pass

# Loading errors:
class LoadingError(SAWException): pass
class CantLoadLLVMModule(LoadingError): pass

# Verification errors:
class VerificationError(SAWException): pass

# Cryptol errors:
class CryptolError(SAWException): pass

# The canonical mapping from Argo error codes to SAW exceptions:
error_code_table : Dict[int, Type[SAWException]] = {
    # Server value errors:
    10000: NoServerValue,
    10010: NotACryptolEnvironment,
    10020: NotAnLLVMModule,
    10030: NotAnLLVMSetupScript,
    10040: NotAnLLVMMethodSpecification,
    # Setup errors:
    10100: NotSettingUpCryptol,
    10110: NotSettingUpCrucibleLLVM,
    10120: NotAtTopLevel,
    # Loading errors:
    10200: CantLoadLLVMModule,
    # Verification errors:
    10300: VerificationError,
    # Cryptol errors:
    11000: CryptolError,
}
