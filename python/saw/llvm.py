from abc import ABCMeta, abstractmethod
from cryptol import cryptoltypes
from saw.llvm_types import LLVMType
from dataclasses import dataclass, field
import pprint
import re
from typing import Any, List, Optional, Set, Union
from typing_extensions import Literal
import inspect
import uuid

class SetupVal(metaclass=ABCMeta):
    """Represent a ``SetupValue`` in SawScript, which "corresponds to
    values that can occur during symbolic execution, which includes both 'Term'
    values, pointers, and composite types consisting of either of these
    (both structures and arrays)."
    """
    @abstractmethod
    def to_json(self) -> Any:
        """JSON representation for this ``SetupVal`` (i.e., how it is represented in expressions, etc).

        N.B., should be a JSON object with a ``'setup value'`` field with a unique tag which the
        server will dispatch on to then interpret the rest of the JSON object.``"""
        pass

class NamedSetupVal(SetupVal):
    """Represents those ``SetupVal``s which are a named reference to some value, e.e., a variable
    or reference to allocated memory."""
    @abstractmethod
    def to_init_json(self) -> Any:
        """JSON representation with the information for those ``SetupVal``s which require additional
        information to initialize/allocate them vs that which is required later to reference them.

        I.e., ``.to_json()`` will be used to refer to such ``SetupVal``s in expressions, and
        ``.to_init_json() is used to initialize/allocate them.``
        """
        pass

class CryptolTerm(SetupVal):
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

    def __repr__(self) -> str:
        return f"CryptolTerm({self.expression!r})"

    def to_json(self) -> Any:
        return {'setup value': 'Cryptol', 'expression': cryptoltypes.to_cryptol(self.expression)}

    def __to_cryptol__(self, ty : Any) -> Any:
        return self.expression.__to_cryptol__(ty)

class FreshVar(NamedSetupVal):
    name : Optional[str]

    def __init__(self, spec : 'Contract', type : LLVMType, suggested_name : Optional[str] = None) -> None:
        self.__name = suggested_name
        self.spec = spec
        self.type = type

    def __to_cryptol__(self, ty : Any) -> Any:
        return cryptoltypes.CryptolLiteral(self.name()).__to_cryptol__(ty)

    def to_init_json(self) -> Any:
        #FIXME it seems we don't actually use two names ever... just the one...do we actually need both?
        name = self.name()
        return {"server name": name,
                "name": name,
                "type": self.type.to_json()}

    def name(self) -> str:
        if self.__name is None:
            self.__name = self.spec.get_fresh_name()
        return self.__name

    def to_json(self) -> Any:
        return {'setup value': 'named', 'name': self.name()}

    def __gt__(self, other : cryptoltypes.CryptolJSON) -> CryptolTerm:
        gt = CryptolTerm("(>)")
        return gt(self, other)

    def __lt__(self, other : cryptoltypes.CryptolJSON) -> CryptolTerm:
        lt = CryptolTerm("(<)")
        return lt(self, other)


class Allocated(NamedSetupVal):
    name : Optional[str]

    def __init__(self, spec : 'Contract', type : LLVMType, *, mutable : bool = True) -> None:
        self.name = None
        self.spec = spec
        self.type = type
        self.mutable = mutable

    def to_init_json(self) -> Any:
        if self.name is None:
            self.name = self.spec.get_fresh_name()
        return {"server name": self.name,
                "type": self.type.to_json(),
                "mutable": self.mutable,
                "alignment": None}

    def to_json(self) -> Any:
        if self.name is None:
            self.name = self.spec.get_fresh_name()
        return {'setup value': 'named', 'name': self.name}

class StructVal(SetupVal):
    fields : List[SetupVal]

    def __init__(self, fields : List[SetupVal]) -> None:
        self.fields = fields

    def to_json(self) -> Any:
        return {'setup value': 'struct', 'fields': [fld.to_json() for fld in self.fields]}

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


class PointsTo:
    def __init__(self, pointer : SetupVal, target : SetupVal) -> None:
        self.pointer = pointer
        self.target = target

    def to_json(self) -> Any:
        return {"pointer": self.pointer.to_json(),
                "points to": self.target.to_json()}


class Condition:
    def __init__(self, condition : CryptolTerm) -> None:
        self.cryptol_term = condition

    def to_json(self) -> Any:
        return cryptoltypes.to_cryptol(self.cryptol_term)


@dataclass
class State:
    contract : 'Contract'
    fresh : List[FreshVar] = field(default_factory=list)
    conditions : List[CryptolTerm] = field(default_factory=list)
    allocated : List[Allocated] = field(default_factory=list)
    points_to : List[PointsTo] = field(default_factory=list)

    def to_json(self) -> Any:
        return {'variables': [v.to_init_json() for v in self.fresh],
                'conditions': [c.to_json() for c in self.conditions],
                'allocated': [a.to_init_json() for a in self.allocated],
                'points to': [p.to_json() for p in self.points_to]
               }

ContractState = \
  Union[Literal['pre'],
        Literal['post'],
        Literal['done']]

@dataclass
class Void:
    def to_json(self) -> Any:
        return None

void = Void()

@dataclass
class VerifyResult:
    contract : 'Contract'
    lemma_name : str

# Lemma names are generated deterministically with respect to a
# particular Python execution trace. This means that re-running the
# same script will be fast when using caching, but REPL-style usage
# will be slow, invalidating the cache at each step. We should be
# smarter about this.
used_lemma_names = set([]) # type: Set[str]

class Contract:
    __used_names : Set[str]

    __state : ContractState = 'pre'

    __pre_state : State
    __post_state : State

    __returns : Optional[Union[SetupVal, Void]]

    __arguments : Optional[List[SetupVal]]

    __definition_lineno : Optional[int]
    __definition_filename : Optional[str]
    __unique_id : uuid.UUID
    __cached_json : Optional[Any]

    def __init__(self) -> None:
        self.__pre_state = State(self)
        self.__post_state = State(self)
        self.__used_names = set()
        self.__arguments = None
        self.__returns = None
        self.__unique_id = uuid.uuid4()
        self.__cached_json = None
        frame = inspect.currentframe()
        if frame is not None and frame.f_back is not None:
            self.__definition_lineno = frame.f_back.f_lineno
            self.__definition_filename = frame.f_back.f_code.co_filename
        else:
            self.__definition_lineno = None
            self.__definition_filename = None

    # To be overridden by users
    def specification(self) -> None:
        pass

    def execute_func(self, *args : SetupVal) -> None:
        """Denotes the end of the precondition specification portion of this ``Contract``, records that
        the function is executed with arguments ``args``, and denotes the beginning of the postcondition
        portion of this ``Contract``."""
        if self.__arguments is not None:
            raise ValueError("The function has already been called once during the specification.")
        elif self.__state is not 'pre':
            raise ValueError("Contract state expected to be 'pre', but found {self.__state!r} (has `execute_func` already been called for this contract?).")
        else:
            self.__arguments = [arg for arg in args]
        self.__state = 'post'

    def get_fresh_name(self, hint : str = 'x') -> str:
        new_name = uniquify(hint, self.__used_names)
        self.__used_names.add(new_name)
        return new_name

    def fresh_var(self, type : LLVMType, suggested_name : Optional[str] = None) -> FreshVar:
        """Declares a fresh variable of type ``type`` (with name ``suggested_name`` if provided and available)."""
        fresh_name = self.get_fresh_name('x' if suggested_name is None else self.get_fresh_name(suggested_name))
        v = FreshVar(self, type, fresh_name)
        if self.__state == 'pre':
            self.__pre_state.fresh.append(v)
        elif self.__state == 'post':
            self.__post_state.fresh.append(v)
        else:
            raise Exception("wrong state")
        return v

    def alloc(self, type : LLVMType, *, read_only : bool = False, points_to : Optional[SetupVal] = None) -> SetupVal:
        """Allocates a pointer of type ``type``.

        If ``read_only == True`` then the allocated memory is immutable.

        If ``points_to != None``, it will also be asserted that the allocated memory contains the
        value specified by ``points_to``.

        :returns A pointer of the proper type to the allocated region."""
        a = Allocated(self, type, mutable = not read_only)
        if self.__state == 'pre':
            self.__pre_state.allocated.append(a)
        elif self.__state == 'post':
            self.__post_state.allocated.append(a)
        else:
            raise Exception("wrong state")

        if points_to != None:
            self.points_to(a, points_to)

        return a

    def points_to(self, pointer : SetupVal, target : SetupVal) -> None:
        pt = PointsTo(pointer, target)
        if self.__state == 'pre':
            self.__pre_state.points_to.append(pt)
        elif self.__state == 'post':
            self.__post_state.points_to.append(pt)
        else:
            raise Exception("wrong state")

    def proclaim(self, condition : Union[str, CryptolTerm, cryptoltypes.CryptolJSON]) -> None:
        if not isinstance(condition, CryptolTerm):
            condition = Condition(CryptolTerm(condition))
        else:
            condition = Condition(condition)
        if self.__state == 'pre':
            self.__pre_state.conditions.append(condition)
        elif self.__state == 'post':
            self.__post_state.conditions.append(condition)
        else:
            raise Exception("wrong state")

    def returns(self, val : Union[Void,SetupVal]) -> None:
        if self.__state == 'post':
            if self.__returns is None:
                self.__returns = val
            else:
                raise ValueError("Return value already specified")
        else:
            raise ValueError("Not in postcondition")

    def lemma_name(self, hint  : Optional[str] = None) -> str:
        if hint is None:
            hint = self.__class__.__name__

        name = uniquify('lemma_' + hint, used_lemma_names)

        used_lemma_names.add(name)

        return name

    def definition_lineno(self) -> Optional[int]:
        return self.__definition_lineno

    def definition_filename(self) -> Optional[str]:
        return self.__definition_filename

    def to_json(self) -> Any:
        if self.__cached_json is not None:
            return self.__cached_json
        else:
            if self.__state != 'pre':
                raise Exception(f'Internal error: wrong contract state -- expected \'pre\', but got: {self.__state!r}')

            self.specification()

            if self.__state != 'post':
                raise Exception(f'Internal error: wrong contract state -- expected \'post\', but got: {self.__state!r}')

            self.__state = 'done'

            if self.__returns is None:
                raise Exception("forgot return")

            self.__cached_json = \
                {'pre vars': [v.to_init_json() for v in self.__pre_state.fresh],
                 'pre conds': [c.to_json() for c in self.__pre_state.conditions],
                 'pre allocated': [a.to_init_json() for a in self.__pre_state.allocated],
                 'pre points tos': [pt.to_json() for pt in self.__pre_state.points_to],
                 'argument vals': [a.to_json() for a in self.__arguments] if self.__arguments is not None else [],
                 'post vars': [v.to_init_json() for v in self.__post_state.fresh],
                 'post conds': [c.to_json() for c in self.__post_state.conditions],
                 'post allocated': [a.to_init_json() for a in self.__post_state.allocated],
                 'post points tos': [pt.to_json() for pt in self.__post_state.points_to],
                 'return val': self.__returns.to_json()}

            return self.__cached_json



# FIXME Is `Any` too permissive here -- can we be a little more precise?
def cryptol(data : Any) -> 'CryptolTerm':
    """Returns a ``CryptolTerm`` wrapper around ``data``."""
    return CryptolTerm(data)

def struct(*fields : SetupVal) -> StructVal:
    """Returns a ``StructVal`` with fields ``fields``."""
    return StructVal(fields)
