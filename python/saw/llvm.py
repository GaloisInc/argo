from abc import ABCMeta, abstractmethod
from cryptol import cryptoltypes
from dataclasses import dataclass, field
import pprint
import re
from typing import Any, List, Optional, Set, Union
from typing_extensions import Literal

class LLVMType(metaclass=ABCMeta):
    @abstractmethod
    def to_json(self) -> Any: pass

class LLVMIntType(LLVMType):
    def __init__(self, width : int) -> None:
        self.width = width

    def to_json(self) -> Any:
        return {'type': 'int', 'width': self.width}

class LLVMPointerType(LLVMType):
    def __init__(self, points_to : LLVMType) -> None:
        self.points_to = points_to

    def to_json(self) -> Any:
        return {'type': 'pointer', 'points to': self.points_to.to_json()}


uint32_t = LLVMIntType(32)

class SetupVal(metaclass=ABCMeta):
    @abstractmethod
    def to_json(self) -> Any: pass

class SetupTerm(SetupVal):
    pass

class CryptolTerm(SetupTerm):
    expression : cryptoltypes.CryptolJSON

    def __init__(self, code : Union[str, cryptoltypes.CryptolJSON]):
        if isinstance(code, str):
            self.expression = cryptoltypes.CryptolLiteral(code)
        else:
            self.expression = code

    def __call__(self, *args : cryptoltypes.CryptolJSON) -> 'CryptolTerm':
        out_term = self.expression
        for a in args:
            out_term = cryptoltypes.CryptolApplication(out_term, a)

        return CryptolTerm(out_term)

    def to_json(self) -> Any:
        return cryptoltypes.to_cryptol(self.expression)

    def __to_cryptol__(self, _ty : Any) -> Any:
        return self.expression

class FreshVar(SetupTerm):
    name : Optional[str]

    def __init__(self, spec : 'Contract', type : LLVMType) -> None:
        self.name = None
        self.spec = spec
        self.type = type

    def __to_cryptol__(self, ty : Any) -> Any:
        if self.name is None:
            self.name = self.spec.get_fresh_name()
        return cryptoltypes.CryptolLiteral(self.name).__to_cryptol__(ty)

    def to_json(self) -> Any:
        if self.name is None:
            self.name = self.spec.get_fresh_name()
        return {'fresh': self.name,
                'type': self.type.to_json()}

name_regexp = re.compile('^(?P<prefix>.*[^0-9])?(?P<number>[0-9]+)?$')

def next_name(x : str) -> str:
    match = name_regexp.match(x)
    if match is None:
        return 'x'
    prefix, number = match.group('prefix', 'number')

    if prefix is None:
        prefix = 'x'
    if number is None:
        next_number = 0
    else:
        next_number = int(number) + 1
    return f'{prefix}{next_number}'


def uniquify(x : str, used : Set[str]) -> str:
    while x in used:
        x = next_name(x)
    return x

class Prop(metaclass=ABCMeta):
    @abstractmethod
    def to_json(self) -> Any: pass

class PointsTo(Prop):
    def __init__(self, pointer : SetupVal, target : SetupVal) -> None:
        self.pointer = pointer
        self.target = target

    def to_json(self) -> Any:
        return {'pointer': self.pointer.to_json(), 'target': self.target.to_json()}

@dataclass
class Allocated:
    type : LLVMType
    name : Optional[str] = None

    def to_json(self) -> Any:
        return {'name': self.name, 'type': self.type.to_json()}

@dataclass
class State:
    contract : 'Contract'
    fresh : List[FreshVar] = field(default_factory=list)
    conditions : List[Prop] = field(default_factory=list)
    allocated : List[Allocated] = field(default_factory=list)
    points_to : List[PointsTo] = field(default_factory=list)

    def to_json(self) -> Any:
        return {'variables': [v.to_json() for v in self.fresh],
                'conditions': [c.to_json() for c in self.conditions],
                'allocated': [a.to_json() for a in self.allocated],
                'points to': [p.to_json() for p in self.points_to]
               }

ContractState = \
  Union[Literal['pre'],
        Literal['call'],
        Literal['post'],
        Literal['done']]

@dataclass
class Void:
    def to_json(self) -> Any:
        return None

void = Void()

class Contract:
    __used_names : Set[str]

    __state : ContractState = 'pre'

    __pre_state : State
    __post_state : State

    __returns : Optional[Union[SetupVal, Void]]

    __arguments : Optional[List[SetupVal]]

    __in_post : bool

    def __init__(self) -> None:
        self.__pre_state = State(self)
        self.__post_state = State(self)
        self.__used_names = set()
        self.__arguments = None
        self.__returns = None
        self.__in_post = False

    # To be overridden by users
    def pre(self) -> None:
        pass
    def call(self) -> None:
        pass
    def post(self) -> None:
        pass

    def get_fresh_name(self, hint : str = 'x') -> str:
        new_name = uniquify(hint, self.__used_names)
        self.__used_names.add(new_name)
        return new_name

    def add_default_var_names(self) -> None:
        for x in self.__dict__:
            if isinstance(self.__dict__[x], FreshVar) and self.__dict__[x].name is None:
                new_name = uniquify(x, self.__used_names)
                self.__dict__[x].name = new_name
                self.__used_names.add(new_name)

    def cryptol(self, data : Any) -> CryptolTerm:
        return CryptolTerm(data)


    def declare(self, type : LLVMType) -> FreshVar:
        v = FreshVar(self, type)
        if self.__state == 'pre':
            self.__pre_state.fresh.append(v)
        elif self.__state == 'post':
            self.__post_state.fresh.append(v)
        else:
            raise Exception("wrong state")
        return v


    def declare_pointer(self, type : LLVMType) -> SetupVal:
        return FreshVar(self, LLVMPointerType(type))

    def points_to(self, pointer : SetupVal, target : SetupVal) -> None:
        pt = PointsTo(pointer, target)
        if self.__state == 'pre':
            self.__pre_state.points_to.append(pt)
        elif self.__state == 'post':
            self.__post_state.points_to.append(pt)
        else:
            raise Exception("wrong state")

    def arguments(self, *args : SetupVal) -> None:
        if self.__arguments is not None:
            raise ValueError("The arguments are already specified")
        else:
            self.__arguments = [arg for arg in args]

    def returns(self, val : Union[Void,SetupVal]) -> None:
        if self.__state == 'post':
            if self.__returns is None:
                self.__returns = val
            else:
                raise ValueError("Return value already specified")
        else:
            raise ValueError("Not in postcondition")


    def contract(self) -> Any:
        if self.__state != 'pre':
            raise Exception("Wrong state")
        self.pre()
        self.add_default_var_names()

        self.__state = 'call'
        self.call()

        self.__state = 'post'
        self.post()
        self.add_default_var_names()

        self.__state = 'done'

        if self.__returns is None:
            raise Exception("forgot return")

        return {'pre': self.__pre_state.to_json(),
                'arguments': [a.to_json() for a in self.__arguments] if self.__arguments is not None else [],
                'post': self.__post_state.to_json(),
                'returns': self.__returns.to_json()}




# data Contract cryptolExpr =
#   Contract
#     { preVars :: [(ServerName, Text, Type)]
#     , preConds :: [(LLVMSetupVal cryptolExpr)]
#     , preAllocated :: [ServerName, Type]
#     , prePointsTos :: [(LLVMSetupVal cryptolExpr, LLVMSetupVal cryptolExpr)]
#     , argumentVals :: [LLVMSetupVal cryptolExpr]
#     , postVars :: [(ServerName, Text, Type)]
#     , postConds :: [(LLVMSetupVal cryptolExpr)]
#     , postAllocated :: [ServerName, Type]
#     , postPointsTos :: [(LLVMSetupVal cryptolExpr, LLVMSetupVal cryptolExpr)]
#     , returnVal :: LLVMSetupVal cryptolExpr
#     }


class Incr(Contract):
    def pre(self) -> None:
        self.x = self.declare(uint32_t)

    def call(self) -> None:
        self.arguments(self.x)

    def post(self) -> None:
        add = self.cryptol("(+)")
        self.returns(add(self.x, self.cryptol(1)))

pprint.pprint(Incr().contract())

print('--------------------------')

class Swap(Contract):
    t : LLVMType
    def __init__(self) -> None:
        super().__init__()
        self.t = uint32_t

    def pre(self) -> None:
        self.x = self.declare(self.t)
        self.y = self.declare(self.t)
        self.x_pointer = self.declare_pointer(self.t)
        self.y_pointer = self.declare_pointer(self.t)
        self.points_to(self.x_pointer, self.x)
        self.points_to(self.y_pointer, self.y)

    def call(self) -> None:
        self.arguments()

    def post(self) -> None:
        self.points_to(self.x_pointer, self.y)
        self.points_to(self.y_pointer, self.x)
        self.returns(void)


pprint.pprint(Swap().contract())
