import argo.interaction

class CryptolStartSetup(argo.interaction.Command):
    def __init__(self, connection, name):
        self.method = 'SAW/Cryptol/start setup'
        self.params = {'name': name}
        super(CryptolStartSetup, self).__init__(connection)

    def process_result(self, _res):
        return None

class CryptolLoadFile(argo.interaction.Command):
    def __init__(self, connection, filename):
        self.method = 'SAW/Cryptol/load file'
        self.params = {'file': filename}
        super(CryptolLoadFile, self).__init__(connection)

    def process_result(self, _res):
        return None

class CryptolLoadModule(argo.interaction.Command):
    def __init__(self, connection, module_name):
        self.method = 'SAW/Cryptol/load module'
        self.params = {'module': module_name}
        super(CryptolLoadModule, self).__init__(connection)

    def process_result(self, _res):
        return None

class CryptolFinishSetup(argo.interaction.Command):
    def __init__(self, connection):
        self.method = 'SAW/Cryptol/finish setup'
        self.params = {}
        super(CryptolFinishSetup, self).__init__(connection)

    def process_result(self, _res):
        return None

class LLVMStartSetup(argo.interaction.Command):
    def __init__(self, connection, name):
        self.method = 'SAW/LLVM/start setup'
        self.params = {'name': name}
        super(LLVMStartSetup, self).__init__(connection)

    def process_result(self, _res):
        return None

class LLVMFinishSetup(argo.interaction.Command):
    def __init__(self, connection):
        self.method = 'SAW/LLVM/finish setup'
        self.params = {}
        super(LLVMFinishSetup, self).__init__(connection)

    def process_result(self, _res):
        return None

class LLVMLoadModule(argo.interaction.Command):
    def __init__(self, connection, name, bitcode_file):
        self.method = 'SAW/LLVM/load module'
        self.params = {'name': name, 'bitcode file': bitcode_file}
        super(LLVMLoadModule, self).__init__(connection)

    def process_result(self, _res):
        return None

class LLVMReturn(argo.interaction.Command):
    def __init__(self, connection, return_value):
        self.method = 'SAW/LLVM/return'
        self.params = {'value': return_value}
        super(LLVMReturn, self).__init__(connection)

    def process_result(self, _res):
        return None

class LLVMVerify(argo.interaction.Command):
    def __init__(self, connection, module, function, lemmas, check_sat, setup, tactic, lemma_name):
        self.method = 'SAW/LLVM/verify'
        self.params = {'module': module,
                       'function': function,
                       'lemmas': lemmas,
                       'check sat': check_sat,
                       'setup': setup,
                       'tactic': tactic,
                       'lemma name': lemma_name}
        super(LLVMVerify, self).__init__(connection)

    def process_result(self, _res):
        return None
