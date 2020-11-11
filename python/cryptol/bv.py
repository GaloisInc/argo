
from functools import reduce
from typing import Any, List, Union

class BV:
    """A bitvector, containing a nonnegative bit width `n`
    and corresponding nonnegative integer `v` that is less than `2^n`."""
    def __init__(self, size : int, value : int) -> None:
        if not isinstance(size, int) or size < 0:
            raise ValueError('`size` parameter to BV must be a nonnegative integer but was given {size!r}.')
        self.__size = size
        if not isinstance(value, int) or value < 0 or value.bit_length() > size:
            raise ValueError(f'{value!r} is not a nonnegative integer representable as an unsigned integer with {size!r} BV.')
        self.__value = value

    def hex(self) -> str:
        """Return the (padded) hexadecimal string for the unsigned integer this BV represents.
        
        Note: padding is determined by BV's size, rounding up a single digit
        for widths not evenly divisible by 4."""
        hex_str_width = 2 + (self.__size // 4) + (0 if (self.__size % 4 == 0) else 1)
        return format(self.__value, f'#0{hex_str_width!r}x')

    def __repr__(self) -> str:
        return f"BV({self.__size!r}, {self.hex()})"

    def size(self) -> int:
        """Width of the BV (i.e., the "bit width" of the value)."""
        return self.__size

    def value(self) -> int:
        """The unsigned integer interpretation of the BV."""
        return self.__value

    def __concat_single(self, other : 'BV') -> 'BV':
        if isinstance(other, BV):
            return BV(self.__size + other.__size, (self.__value << other.__size) + other.__value)
        else:
            raise ValueError(f'Cannot concat BV with {other!r}')

    def concat(self, *args : List['BV']) -> 'BV':
        """Concatenate the given BV to the right of `self`."""
        return reduce(lambda acc, b: acc.__concat_single(b), args, self)
    
    @staticmethod
    def join(*bs : List['BV']) -> 'BV':
        """Concatenate the given BV in order."""
        return reduce(lambda acc, b: acc.__concat_single(b), bs, BV(0,0))

    def zero(self) -> 'BV':
        """Return the zero value for this BV' size."""
        return BV(self.size() ,0)

    def to_int(self) -> 'BV':
        """Return the unsigned integer the BV represent."""
        return self.__value

    def to_signed_int(self) -> int:
        """Return the signed (i.e., two's complement) integer the BV represents."""
        if not self.msb():
            return self.__value
        else:
            return 0 - ((2 ** self.__size) - self.__value)

    def msb(self) -> bool:
        """Returns True if the most significant bit is 1, else returns False."""
        if self.__size == 0:
            raise ValueError("0-length BVs have no most significant bit.")
        else:
            return self[self.__size - 1]

    def lsb(self) -> bool:
        """Returns True if the least significant bit is 1, else returns False."""
        if self.__size == 0:
            raise ValueError("0-length BVs have no least significant bit.")
        else:
            return self[0]


    def __eq__(self, other : Any) -> bool:
        if isinstance(other, BV):
            return self.__size == other.__size and self.__value == other.__value
        else:
            return False

    def __index__(self):
        return self.__value

    def __len__(self):
        return self.__size

    def __bytes__(self):
        byte_len = (self.__size // 8) + (0 if self.__size % 8 == 0 else 1)
        return self.__value.to_bytes(byte_len, 'big')


    def split(self, size : int) -> List['BV']:
        """Split the BC into a list of BVs of length `size`.
        
        Note: `self.size()` must be divisible by `size`."""
        if not isinstance(size, int) or size <= 0:
            raise ValueError(f'`size` argument to splits must be a positive integer, got {size!r}')
        if not self.size() % size == 0:
            raise ValueError(f'{self!r} is not divisible into equal parts of size {size!r}')
        mask = (1 << size) - 1
        return [BV(size, (self.__value >> (i * size)) & mask)
                for i in range(self.size() // size - 1, -1, -1)]


    def popcount(self) -> int:
        """Return the number of bits set to `1` in `self`."""
        return bin(self).count("1")

    @staticmethod
    def from_bytes(bs : bytes, *, size=None, byteorder='big') -> 'BV':
        """Convert the given bytes into a BV.

        Resulting BV has length `len(bs) * 8.` 
        
        `size` keyword argument specifies BV's size (and
        must be large enough to represent the bytes). If `size=None`
        the resulting BV's size is `len(bs) * 8`.

        `byteorder` keyword argument can be `little` or `big` to select
        endianness."""

        if not isinstance(bs, bytes):
            raise ValueError("from_bytes given not bytes value: {bs!r}")

        if not byteorder == 'little' and not byteorder == 'big':
            raise ValueError("from_bytes given not bytes value: {bs!r}")

        if size == None:
            return BV(len(bs) * 8, int.from_bytes(bs, byteorder=byteorder))
        elif isinstance(size, int) and size >= len(bs) * 8:
            return BV(size, int.from_bytes(bs, byteorder=byteorder))
        else:
            raise ValueError(f'from_bytes given invalid bit size {size!r} for bytes {bs!r}')

    def with_bit(self, index : int, set_bit : bool) -> 'BV':
        """Return a BV identical to `self` bit with the bit at `index` set to
        `1` if `set_bit == True`, else `0`."""
        if index < 0 or index >= self.__size:
            raise ValueError(f'{index!r} is not a valid bit index for {self!r}')
        if set_bit:
            mask = (1 << index)
            return BV(self.__size, self.__value | mask)
        else:
            mask = (2 ** self.__size - 1) ^ (1 << index)
            return BV(self.__size, self.__value & mask)


    def to_bytes(self) -> bytes:
        """Convert the given BV into a python native `bytes` value.
        
        Note: equivalent to bytes(_)."""

        return self.__bytes__()

    def __mod_if_overflow(self, value):
        return value if value.bit_length() <= self.__size \
               else (value % (2 ** self.__size))

    def __add__(self, other : Union[int, 'BV']) -> 'BV':
        if isinstance(other, BV):
            if self.__size == other.__size:
                return BV(
                    self.__size,
                    self.__mod_if_overflow(self.__value + other.__value))
            else:
                self.__raise_unequal_len_op_error("+", other)
        elif isinstance(other, int):
            return BV(
                self.__size,
                self.__mod_if_overflow(self.__value + other))
        else:
            raise ValueError(f'Cannot add {self!r} with {other!r}.')
    
    def __radd__(self, other : int) -> 'BV':
        if isinstance(other, int):
            return BV(self.__size, self.__mod_if_overflow(self.__value + other))
        else:
            raise ValueError(f'Cannot add {self!r} with {other!r}.')   

    def __and__(self, other : Union['BV', int]) -> 'BV':
        if isinstance(other, BV):
            if self.__size == other.__size:
                return BV(self.__size, self.__value & other.__value)
            else:
                self.__raise_unequal_len_op_error("&", other)
        elif isinstance(other, int):
            return BV(self.__size, self.__value & other)
        else:
            raise ValueError(f'Cannot bitwise and {self!r} with value {other!r}.')

    def __rand__(self, other : int) -> 'BV':
        if isinstance(other, int):
            return BV(self.__size, self.__value & other)
        else:
            raise ValueError(f'Cannot bitwise and {self!r} with value {other!r}.')
    
    def __or__(self, other : Union['BV', int]) -> 'BV':
        if isinstance(other, BV):
            if self.__size == other.__size:
                return BV(self.__size, self.__value | other.__value)
            else:
                self.__raise_unequal_len_op_error("|", other)
        elif isinstance(other, int):
            return BV(self.__size, self.__value | other)
        else:
            raise ValueError(f'Cannot bitwise or {self!r} with value {other!r}.')

    def __ror__(self, other : int) -> 'BV':
        if isinstance(other, int):
            return BV(self.__size, self.__value | other)
        else:
            raise ValueError(f'Cannot bitwise or {self!r} with value {other!r}.')

    def __xor__(self, other : Union['BV', int]) -> 'BV':
        if isinstance(other, BV):
            if self.__size == other.__size:
                return BV(self.__size, self.__value ^ other.__value)
            else:
                self.__raise_unequal_len_op_error("^", other)
        elif isinstance(other, int):
            return BV(self.__size, self.__value ^ other)
        else:
            raise ValueError(f'Cannot bitwise xor {self!r} with value {other!r}.')

    def __rxor__(self, other : int) -> 'BV':
        if isinstance(other, int):
            return BV(self.__size, self.__value ^ other)
        else:
            raise ValueError(f'Cannot bitwise xor {self!r} with value {other!r}.')

    def __getitem__(self, key):
        if isinstance(key, int):
            if key < 0 or key >= self.__size:
                raise ValueError(f'{key!r} is not a valid index for {self!r}')
            else:
                return (self.__value & (1 << key)) != 0
        if isinstance(key, slice):
            high = key.start
            low = key.stop
            if not isinstance(low, int): raise ValueError(f'Expected BV slice to use non-negative integer indices, but got low index of {low!r}.')
            if low < 0 and low > self.__size: raise ValueError(f'Expected BV slice low index to be >= 0 and <= the BV size (i.e., {self.__size!r}) but got {low!r}.')
            if not isinstance(high, int): raise ValueError(f'Expected BV slice to use non-negative integer indices, but got high index of {high!r}.')
            if low > high: raise ValueError(f'BV slice low index {low!r} is larger than the high index {high!r}.')
            if high > self.__size: raise ValueError(f'BV slice high index {high!r} is larger than the BV size (i.e., {self.__size!r}).')
            if key.step: raise ValueError(f'BV slicing expects a step of None, but found {key.step!r}')
            new_sz = high - low
            return BV(new_sz, (self.__value >> low) & ((2 ** new_sz) - 1))
        else:
            raise ValueError(f'{key!r} is not a valid BV index or slice.')

    def __invert__(self) -> 'BV':
        return BV(self.__size, (1 << self.__size) - 1 - self.__value)

    @staticmethod
    def __from_signed_int(size: int, val : int) -> 'BV':
        excl_max = 2 ** size
        if (size == 0):
            return BV(0,0)
        elif val >= 0:
            return BV(size, val % excl_max)
        else:
            return BV(size, ((excl_max - 1) & ~(abs(val + 1))) % excl_max)

    @staticmethod
    def from_signed_int(size: int, val : int) -> 'BV':
        """Convert `val` into the corresponding `size`-bit two's complement bitvector."""
        if size == 0:
            raise ValueError("There are no two's complement 0-bit vectors.")
        max_val = 2 ** (size - 1) - 1
        min_val = -(2 ** (size - 1))
        if val < min_val or val > max_val:
            raise ValueError(f'{val!r} is not in range [{min_val!r},{max_val!r}].')
        else:
            return BV.__from_signed_int(size, val)

    def __sub__(self, other : Union[int, 'BV']) -> 'BV':
        if isinstance(other, BV):
            if self.__size == other.__size:
                if self.__size == 0:
                    return self
                else:
                    return BV.__from_signed_int(
                        self.__size,
                        self.to_signed_int() - other.to_signed_int())
            else:
                self.__raise_unequal_len_op_error("-", other)
        elif isinstance(other, int):
            self.__check_int_size(other)
            if self.__size == 0:
                return self
            else:
                return BV.__from_signed_int(
                    self.__size,
                    self.to_signed_int() - other)
        else:
            raise ValueError(f'Cannot subtract {other!r} from {self!r}.')

    def __rsub__(self, other : int) -> 'BV':
        if isinstance(other, int):
            self.__check_int_size(other)
            if self.__size == 0:
                return self
            else:
                return BV.__from_signed_int(
                        self.__size,
                        other - self.to_signed_int())
        else:
            raise ValueError(f'Cannot subtract {self!r} from {other!r}.')


    def __mul__(self, other: Union[int, 'BV']) -> 'BV':
        if isinstance(other, BV):
            if self.__size == other.__size:
                return BV(
                    self.__size,
                    self.__mod_if_overflow(self.__value * other.__value))
            else:
                self.__raise_unequal_len_op_error("*", other)
        elif isinstance(other, int):
            self.__check_int_size(other)
            return BV.__from_signed_int(
                self.__size, 
                self.__mod_if_overflow(self.__value * other))
        else:
            raise ValueError(f'Cannot multiply {self!r} and {other!r}.')

    def __rmul__(self, other : int) -> 'BV':
        return self.__mul__(other)

    
    def __lshift__(self, other : Union[int, 'BV']) -> 'BV':
        if isinstance(other, int) or isinstance(other, BV):
            n = int(other)
            if n < 0:
                raise ValueError(f'Cannot left shift a negative amount (i.e, {n!r}).')
            return BV(self.__size + n, self.__value << n)
        else:
            raise ValueError(f'Shift must be specified with an integer or BV, but got {other!r}.')


    def __rshift__(self, other : Union[int, 'BV']) -> 'BV':
        if isinstance(other, int) or isinstance(other, BV):
            n = int(other)
            if n < 0:
                raise ValueError(f'Cannot right shift a negative amount (i.e, {n!r}).')
            return BV(max(0, self.__size - n), self.__value >> n)
        else:
            raise ValueError(f'Shift must be specified with an integer or BV, but got {other!r}.')


# object.__rshift__(self, other)


    def __check_int_size(self, val : int) -> None:
        if val >= (2 ** self.__size) or val < 0:
            raise ValueError(f'{val!r} is not a valid unsigned {self.__size!r}-bit value.')


    def __raise_unequal_len_op_error(self, op : str, other : 'BV') -> None:
        raise ValueError(f'Operator `{op}` cannot be called on BV of unequal length {self!r} and {other!r}.')



# object.__matmul__(self, other)

# object.__truediv__(self, other)

# object.__floordiv__(self, other)

# object.__mod__(self, other)

# object.__divmod__(self, other)

# object.__pow__(self, other[, modulo])

# object.__lshift__(self, other)

# object.__rshift__(self, other)

# object.__xor__(self, other)

# extern void bitvector_t_zeroize(bitvector_t *bv);
# extern void bitvector_t_cleanHighBits(bitvector_t *bv);
# extern void bitvector_t_widenUpdate(bitvector_t *bv, uint32_t nBitsToAdd);
# extern bitvector_t *bitvector_t_widen(bitvector_t *bv, uint32_t nBitsToAdd);
# extern uint64_t hexchar_to_digit(char c);
# extern bitvector_t *bitvector_t_fromHexString(char *string);
# extern void bitvector_t_copyUpdate(bitvector_t *dst, bitvector_t *src);
# extern bitvector_t *bitvector_t_copy(bitvector_t *bv);
# extern void bitvector_t_dropUpdate(bitvector_t *bv, uint32_t nBitsToDrop);
# extern bitvector_t *bitvector_t_from_bytes(uint8_t *bytes, uint32_t nBytes);
# extern uint8_t *bitvector_t_to_bytes(bitvector_t *bv);
# extern void bitvector_t_negateUpdate(bitvector_t *bv);
# extern void bitvector_t_sliceUpdate(bitvector_t *slice, bitvector_t *bv, uint32_t start, uint32_t length);
# extern uint8_t bitvector_t_equal(bitvector_t *x, bitvector_t *y);

# #define bitvector_t_zipWith(NAME)                                       \
# extern void bitvector_t_##NAME##Update(bitvector_t *x, bitvector_t *y); \
# extern bitvector_t *bitvector_t_##NAME(bitvector_t *x, bitvector_t *y); \

# bitvector_t_zipWith(xor)
# bitvector_t_zipWith(or)
# bitvector_t_zipWith(and)
