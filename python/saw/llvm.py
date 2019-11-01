from abc import ABCMeta, abstractmethod
import re
from typing import Any, List, Optional, Set

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

class FreshVar(SetupVal):
    name : Optional[str]

    def __init__(self, spec : Spec, type : LLVMType) -> None:
        self.name = None
        self.spec = spec
        self.type = type

    def to_json(self) -> Any:
        if self.name is None:
            raise ValueError('No assigned name for decl')
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

class Spec:
    __used_names : Set[str]

    __pre_props : Set[Prop]
    __post_props : Set[Prop]

    __current_props : Optional[Set[Prop]]

    __arguments : Optional[List[SetupVal]]

    def __init__(self) -> None:
        self.__used_names = set()
        self.__pre_props = set()
        self.__post_props = set()
        self.__current_props = None
        self.__arguments = None

    # To be overridden by users
    def setup(self) -> None:
        pass
    def pre(self) -> None:
        pass
    def call(self) -> None:
        pass
    def post(self) -> None:
        pass



    def declare(self, type : LLVMType) -> SetupVal:
        return FreshVar(self, type)

    def declare_pointer(self, type : LLVMType) -> SetupVal:
        return FreshVar(self, LLVMPointerType(type))

    def points_to(self, pointer : SetupVal, target : SetupVal) -> None:
        if self.__current_props is None:
            raise ValueError("Not in pre- or post-condition setup")
        else:
            self.__current_props.add(PointsTo(pointer, target))

    def arguments(self, *args : SetupVal) -> None:
        if self.__arguments is not None:
            raise ValueError("The arguments are already specified")
        else:
            self.__arguments = [arg for arg in args]

    def contract(self) -> Any:
        self.setup()
        for x in self.__dict__:
            if isinstance(self.__dict__[x], FreshVar) and self.__dict__[x].name is None:
                new_name = uniquify(x, self.__used_names)
                self.__dict__[x].name = new_name
                self.__used_names.add(new_name)

        self.__current_props = self.__pre_props
        self.pre()

        self.__current_props = None
        self.call()

        self.__current_props = self.__post_props
        self.post()

        return {'vars' : [d.to_json()
                          for d in self.__dict__.values()
                          if isinstance(d, SetupVal)]}


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


class Incr(Spec):
    def setup(self) -> None:
        self.x = self.declare(uint32_t)

print(Incr().contract())

class Swap(Spec):
    def setup(self) -> None:
        t = uint32_t
        self.x = self.declare(t)
        self.y = self.declare(t)
        self.x_pointer = self.declare_pointer(t)
        self.y_pointer = self.declare_pointer(t)

    def pre(self) -> None:
        self.points_to(self.x_pointer, self.x)
        self.points_to(self.y_pointer, self.y)

    def call(self) -> None:
        self.arguments()

    def post(self) -> None:
        self.points_to(self.x_pointer, self.y)
        self.points_to(self.y_pointer, self.x)


print(Swap().contract())
