"""Cryptol bindings from Python. Use :py:func:`cryptol.connect` to start a new connection."""

from __future__ import annotations

import base64
import os
import types
import sys
from typing import Any, Dict, Iterable, List, Mapping, NoReturn, Optional, Type, Union
from mypy_extensions import TypedDict

import argo.interaction
from argo.interaction import HasProtocolState
from argo.connection import DynamicSocketProcess, ServerConnection, ServerProcess, StdIOProcess
from . import cryptoltypes
from . import solver
from cryptol.bitvector import BV


__all__ = ['cryptoltypes', 'solver']



# Current status:
#  It can currently launch a server, given a suitable command line as an argument. Try this:
#  >>> c = CryptolConnection("cabal -v0 run cryptol-remote-api")
#  >>> f = c.load_file(FILE)
#  >>> f.result()
#



def extend_hex(string : str) -> str:
    if len(string) % 2 == 1:
        return '0' + string
    else:
        return string

def fail_with(x : Exception) -> NoReturn:
    "Raise an exception. This is valid in expression positions."
    raise x



def from_cryptol_arg(val : Any) -> Any:
    """Return the canonical Python value for a Cryptol JSON value."""
    if isinstance(val, bool):
        return val
    elif isinstance(val, int):
        return val
    elif 'expression' in val.keys():
        tag = val['expression']
        if tag == 'unit':
            return ()
        elif tag == 'tuple':
            return tuple(from_cryptol_arg(x) for x in val['data'])
        elif tag == 'record':
            return {k : from_cryptol_arg(val[k]) for k in val['data']}
        elif tag == 'sequence':
            return [from_cryptol_arg(v) for v in val['data']]
        elif tag == 'bits':
            enc = val['encoding']
            size = val['width']
            if enc == 'base64':
                n = int.from_bytes(
                        base64.b64decode(val['data'].encode('ascii')),
                        byteorder='big')
            elif enc == 'hex':
                n = int.from_bytes(
                    bytes.fromhex(extend_hex(val['data'])),
                    byteorder='big')
            else:
                raise ValueError("Unknown encoding " + str(enc))
            return BV(size, n)
        else:
            raise ValueError("Unknown expression tag " + tag)
    else:
        raise TypeError("Unsupported value " + str(val))



class CryptolChangeDirectory(argo.interaction.Command):
    def __init__(self, connection : HasProtocolState, new_directory : str) -> None:
        super(CryptolChangeDirectory, self).__init__(
            'change directory',
            {'directory': new_directory},
            connection
        )

    def process_result(self, res : Any) -> Any:
        return res

class CryptolLoadModule(argo.interaction.Command):
    def __init__(self, connection : HasProtocolState, mod_name : str) -> None:
        super(CryptolLoadModule, self).__init__('load module', {'module name': mod_name}, connection)

    def process_result(self, res : Any) -> Any:
        return res

class CryptolLoadFile(argo.interaction.Command):
    def __init__(self, connection : HasProtocolState, filename : str) -> None:
        super(CryptolLoadFile, self).__init__('load file', {'file': filename}, connection)

    def process_result(self, res : Any) -> Any:
        return res


class CryptolEvalExpr(argo.interaction.Query):
    def __init__(self, connection : HasProtocolState, expr : Any) -> None:
        super(CryptolEvalExpr, self).__init__(
            'evaluate expression',
            {'expression': expr},
            connection
        )

    def process_result(self, res : Any) -> Any:
        return res

class CryptolCall(argo.interaction.Query):
    def __init__(self, connection : HasProtocolState, fun : str, args : List[Any]) -> None:
        super(CryptolCall, self).__init__(
            'call',
            {'function': fun, 'arguments': args},
            connection
        )

    def process_result(self, res : Any) -> Any:
        return from_cryptol_arg(res['value'])

class CryptolCheckType(argo.interaction.Query):
    def __init__(self, connection : HasProtocolState, expr : Any) -> None:
        super(CryptolCheckType, self).__init__(
            'check type',
            {'expression': expr},
            connection
        )

    def process_result(self, res : Any) -> Any:
        return res['type schema']

class CryptolProveSat(argo.interaction.Query):
    def __init__(self, connection : HasProtocolState, qtype : str, expr : Any, solver : solver.Solver, count : Optional[int]) -> None:
        super(CryptolProveSat, self).__init__(
            'prove or satisfy',
            {'query type': qtype,
             'expression': expr,
             'prover': solver,
             'result count': 'all' if count is None else count},
            connection
        )
        self.qtype = qtype

    def process_result(self, res : Any) -> Any:
        if res['result'] == 'unsatisfiable':
            if self.qtype == 'sat':
                return False
            elif self.qtype == 'prove':
                return True
            else:
                raise ValueError("Unknown prove/sat query type: " + self.qtype)
        elif res['result'] == 'invalid':
            return [from_cryptol_arg(arg['expr'])
                    for arg in res['counterexample']]
        elif res['result'] == 'satisfied':
            return [from_cryptol_arg(arg['expr'])
                    for m in res['models']
                    for arg in m]
        else:
            raise ValueError("Unknown sat result " + str(res))

class CryptolProve(CryptolProveSat):
    def __init__(self, connection : HasProtocolState, expr : Any, solver : solver.Solver) -> None:
        super(CryptolProve, self).__init__(connection, 'prove', expr, solver, 1)

class CryptolSat(CryptolProveSat):
    def __init__(self, connection : HasProtocolState, expr : Any, solver : solver.Solver, count : int) -> None:
        super(CryptolSat, self).__init__(connection, 'sat', expr, solver, count)

class CryptolNames(argo.interaction.Query):
    def __init__(self, connection : HasProtocolState) -> None:
        super(CryptolNames, self).__init__('visible names', {}, connection)

    def process_result(self, res : Any) -> Any:
        return res

class CryptolFocusedModule(argo.interaction.Query):
    def __init__(self, connection : HasProtocolState) -> None:
        super(CryptolFocusedModule, self).__init__(
            'focused module',
            {},
            connection
        )

    def process_result(self, res : Any) -> Any:
        return res

def connect(command : str, cryptol_path : Optional[str] = None) -> CryptolConnection:
    """Start a new connection to a new Cryptol server process.

    :param command: The command to launch the Cryptol server.

    :param cryptol_path: An optional replacement for the contents of
      the ``CRYPTOLPATH`` environment variable.

    """
    return CryptolConnection(command, cryptol_path)

def connect_stdio(command : str, cryptol_path : Optional[str] = None) -> CryptolConnection:
    """Start a new connection to a new Cryptol server process.

    :param command: The command to launch the Cryptol server.

    :param cryptol_path: An optional replacement for the contents of
      the ``CRYPTOLPATH`` environment variable.

    """
    conn = CryptolStdIOProcess(command, cryptol_path=cryptol_path)
    return CryptolConnection(conn)


class CryptolConnection:
    """Instances of ``CryptolConnection`` represent a particular point of
    time in an interaction with Cryptol. Issuing a command through a
    ``CryptolConnection`` causes it to change its state into one in
    which the command has been carried out.

    Use :py:meth:`cryptol.CryptolConnection.snapshot` to make a
    lightweight copy of the current state that shares the underlying
    server process.

    ``CryptolConnection`` is in the middle of the abstraction
    hierarchy. It relieves users from thinking about explicitly
    encoding state and protocol messages, but it provides a
    non-blocking interface in which answers from Cryptol must be
    explicitly requested. Note that blocking may occur in the case of
    sequential state dependencies between commands.
    """
    most_recent_result : Optional[argo.interaction.Interaction]
    proc: ServerProcess

    def __init__(self, command_or_connection : Union[str, ServerConnection, ServerProcess], cryptol_path : Optional[str] = None) -> None:
        self.most_recent_result = None
        if isinstance(command_or_connection, ServerProcess):
            self.proc = command_or_connection
            self.server_connection = ServerConnection(self.proc)
        elif isinstance(command_or_connection, str):
            self.proc = CryptolDynamicSocketProcess(command_or_connection, cryptol_path=cryptol_path)
            self.server_connection = ServerConnection(self.proc)
        else:
            self.server_connection = command_or_connection

    def snapshot(self) -> CryptolConnection:
        """Create a lightweight snapshot of this connection. The snapshot
        shares the underlying server process, but can have different
        application state.
        """
        copy = CryptolConnection(self.server_connection)
        copy.most_recent_result = self.most_recent_result
        return copy

    def protocol_state(self) -> Any:
        if self.most_recent_result is None:
            return None
        else:
            return self.most_recent_result.state()

    # Protocol messages
    def change_directory(self, new_directory : str) -> argo.interaction.Command:
        """Change the working directory of the Cryptol process."""
        self.most_recent_result = CryptolChangeDirectory(self, new_directory)
        return self.most_recent_result

    def load_file(self, filename : str) -> argo.interaction.Command:
        """Load a filename as a Cryptol module, like ``:load`` at the Cryptol
        REPL.
        """
        self.most_recent_result = CryptolLoadFile(self, filename)
        return self.most_recent_result

    def load_module(self, module_name : str) -> argo.interaction.Command:
        """Load a Cryptol module, like ``:module`` at the Cryptol REPL."""
        self.most_recent_result = CryptolLoadModule(self, module_name)
        return self.most_recent_result

    def evaluate_expression(self, expression : Any) -> argo.interaction.Query:
        """Evaluate a Cryptol expression, represented according to
        :ref:`cryptol-json-expression`, with Python datatypes standing
        for their JSON equivalents.
        """
        self.most_recent_result = CryptolEvalExpr(self, expression)
        return self.most_recent_result

    def call(self, fun : str, *args : List[Any]) -> argo.interaction.Query:
        encoded_args = [cryptoltypes.CryptolType().from_python(a) for a in args]
        self.most_recent_result = CryptolCall(self, fun, encoded_args)
        return self.most_recent_result

    def check_type(self, code : Any) -> argo.interaction.Query:
        """Check the type of a Cryptol expression, represented according to
        :ref:`cryptol-json-expression`, with Python datatypes standing for
        their JSON equivalents.
        """
        self.most_recent_result = CryptolCheckType(self, code)
        return self.most_recent_result

    def sat(self, expr : Any, solver : solver.Solver = solver.Z3, count : int = 1) -> argo.interaction.Query:
        """Check the satisfiability of a Cryptol expression, represented according to
        :ref:`cryptol-json-expression`, with Python datatypes standing for
        their JSON equivalents. Use the solver named `solver`, and return up to
        `count` solutions.
        """
        self.most_recent_result = CryptolSat(self, expr, solver, count)
        return self.most_recent_result

    def prove(self, expr : Any, solver : solver.Solver = solver.Z3) -> argo.interaction.Query:
        """Check the validity of a Cryptol expression, represented according to
        :ref:`cryptol-json-expression`, with Python datatypes standing for
        their JSON equivalents. Use the solver named `solver`.
        """
        self.most_recent_result = CryptolProve(self, expr, solver)
        return self.most_recent_result

    def names(self) -> argo.interaction.Query:
        """Discover the list of names currently in scope in the current context."""
        self.most_recent_result = CryptolNames(self)
        return self.most_recent_result

    def focused_module(self) -> argo.interaction.Query:
        """Return the name of the currently-focused module."""
        self.most_recent_result = CryptolFocusedModule(self)
        return self.most_recent_result



class CryptolDynamicSocketProcess(DynamicSocketProcess):

    def __init__(self, command: str, *,
                 persist: bool=False,
                 cryptol_path: Optional[str]=None):

        environment = os.environ.copy()
        if cryptol_path is not None:
            environment["CRYPTOLPATH"] = str(cryptol_path)

        super().__init__(command, persist=persist, environment=environment)

class CryptolStdIOProcess(StdIOProcess):

    def __init__(self, command: str, *,
                 cryptol_path: Optional[str]=None):

        environment = os.environ.copy()
        if cryptol_path is not None:
            environment["CRYPTOLPATH"] = str(cryptol_path)

        super().__init__(command, environment=environment)



class CryptolFunctionHandle:
    def __init__(self,
                 connection : CryptolConnection,
                 name : str,
                 ty : Any,
                 schema : Any,
                 docs : Optional[str] = None) -> None:
        self.connection = connection.snapshot()
        self.name = name
        self.ty = ty
        self.schema = schema
        self.docs = docs

        self.__doc__ = "Cryptol type: " + ty
        if self.docs is not None and self.__doc__ is not None:
            self.__doc__ += "\n" + self.docs

    def __call__(self, *args : List[Any]) -> Any:
        current_type = self.schema
        remaining_args = args
        arg_types = cryptoltypes.argument_types(current_type)
        Call = TypedDict('Call', {'expression': str, 'function': str, 'arguments': List[Any]})
        current_expr : Union[str, Call]
        current_expr = self.name
        found_args = []
        while len(arg_types) > 0 and len(remaining_args) > 0:
            found_args.append(arg_types[0].from_python(remaining_args[0]))
            current_expr = {'expression': 'call', 'function': self.name, 'arguments': found_args}
            ty = self.connection.check_type(current_expr).result()
            current_type = cryptoltypes.to_schema(ty)
            arg_types = cryptoltypes.argument_types(current_type)
            remaining_args = remaining_args[1:]
        return from_cryptol_arg(self.connection.evaluate_expression(current_expr).result()['value'])


def cry(string : str) -> cryptoltypes.CryptolCode:
    return cryptoltypes.CryptolLiteral(string)

class CryptolModule(types.ModuleType):
    def __init__(self, connection : CryptolConnection) -> None:
        self.connection = connection.snapshot()
        name = connection.focused_module().result()
        if name["module"] is None:
            raise ValueError("Provided connection is not in a module")
        super(CryptolModule, self).__init__(name["module"])

        for x in self.connection.names().result():
            if 'documentation' in x:
                setattr(self, x['name'],
                        CryptolFunctionHandle(self.connection,
                                              x['name'],
                                              x['type string'],
                                              cryptoltypes.to_schema(x['type']),
                                              x['documentation']))
            else:
                setattr(self, x['name'],
                        CryptolFunctionHandle(self.connection,
                                              x['name'],
                                              x['type string'],
                                              cryptoltypes.to_schema(x['type'])))


def add_cryptol_module(name : str, connection : CryptolConnection) -> None:
    """Given a name for a Python module and a Cryptol connection,
    establish a Python module with the given name in which all the
    Cryptol names are in scope as Python proxy objects.
    """
    sys.modules[name] = CryptolModule(connection)

class CryptolContext:
    _defined : Dict[str, CryptolFunctionHandle]

    def __init__(self, connection : CryptolConnection) -> None:
        self.connection = connection.snapshot()
        self._defined = {}
        for x in self.connection.names().result():
            if 'documentation' in x:
                self._defined[x['name']] = \
                    CryptolFunctionHandle(self.connection,
                                          x['name'],
                                          x['type string'],
                                          cryptoltypes.to_schema(x['type']),
                                          x['documentation'])
            else:
                self._defined[x['name']] = \
                    CryptolFunctionHandle(self.connection,
                                          x['name'],
                                          x['type string'],
                                          cryptoltypes.to_schema(x['type']))

    def __dir__(self) -> Iterable[str]:
        return self._defined.keys()

    def __getattr__(self, name : str) -> Any:
        if name in self._defined:
            return self._defined[name]
        else:
            raise AttributeError()
