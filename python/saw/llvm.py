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
        return {'type': 'primitive type', 'primitive': 'integer', 'size': self.width}

class LLVMArrayType(LLVMType):
    def __init__(self, elemtype : LLVMType, size : int) -> None:
        self.size = size
        self.elemtype = elemtype

    def to_json(self) -> Any:
        return { 'type': 'array',
                 'element type': self.elemtype.to_json(),
                 'size': self.size }

class LLVMPointerType(LLVMType):
    def __init__(self, points_to : LLVMType) -> None:
        self.points_to = points_to

    def to_json(self) -> Any:
        return {'type': 'pointer', 'points to': self.points_to.to_json()}


uint8_t = LLVMIntType(8)
uint32_t = LLVMIntType(32)

class SetupVal(metaclass=ABCMeta):
    @abstractmethod
    def to_json(self) -> Any: pass

    @abstractmethod
    def to_ref_json(self) -> Any: pass

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

    def to_ref_json(self) -> Any:
        return {'setup value': 'Cryptol', 'expression': self.to_json()}

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
        return [self.name, self.name, self.type.to_json()]

    def to_ref_json(self) -> Any:
        if self.name is None:
            self.name = self.spec.get_fresh_name()
        return {'setup value': 'saved', 'name': self.name}

class Allocated(SetupTerm):
    name : Optional[str]

    def __init__(self, spec : 'Contract', type : LLVMType) -> None:
        self.name = None
        self.spec = spec
        self.type = type

    def to_json(self) -> Any:
        if self.name is None:
            self.name = self.spec.get_fresh_name()
        return [self.name, self.type.to_json()]

    def to_ref_json(self) -> Any:
        if self.name is None:
            self.name = self.spec.get_fresh_name()
        return {'setup value': 'saved', 'name': self.name}

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

class PointsTo:
    def __init__(self, pointer : SetupVal, target : SetupVal) -> None:
        self.pointer = pointer
        self.target = target

    def to_json(self) -> Any:
        return [self.pointer.to_ref_json(), self.target.to_ref_json()]



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
            if isinstance(self.__dict__[x], Allocated) and self.__dict__[x].name is None:
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
        a = Allocated(self, type)
        if self.__state == 'pre':
            self.__pre_state.allocated.append(a)
        elif self.__state == 'post':
            self.__post_state.allocated.append(a)
        else:
            raise Exception("wrong state")
        return a

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

        return {'pre vars': [v.to_json() for v in self.__pre_state.fresh],
                'pre conds': [c.to_json() for c in self.__pre_state.conditions],
                'pre allocated': [a.to_json() for a in self.__pre_state.allocated],
                'pre points tos': [pt.to_json() for pt in self.__pre_state.points_to],
                'argument vals': [a.to_ref_json() for a in self.__arguments] if self.__arguments is not None else [],
                'post vars': [v.to_json() for v in self.__post_state.fresh],
                'post conds': [c.to_json() for c in self.__post_state.conditions],
                'post allocated': [a.to_json() for a in self.__post_state.allocated],
                'post points tos': [pt.to_json() for pt in self.__post_state.points_to],
                'return val': self.__returns.to_json()}

