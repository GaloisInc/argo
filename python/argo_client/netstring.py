"""Argo uses D. J. Berstein's `netstrings <https://cr.yp.to/proto/netstrings.txt>`_
as a lightweight transport layer for JSON RPC.
"""

from typing import Optional, Tuple

def encode(string : str) -> bytes:
    """Encode a ``str`` into a netstring.

    >>> encode("hello")
    b'5:hello,'
    """
    bytestring = string.encode()
    return str(len(bytestring)).encode() + b':' + bytestring + b','

class InvalidNetstring(Exception):
    """Exception for malformed netstrings"""
    def __init__(self, message: str):
        self.message = message
        super().__init__(self.message)

def decode(netstring : bytes) -> Optional[Tuple[str, bytes]]:
    """Decode the first valid netstring from a bytestring, returning its
    string contents and the remainder of the bytestring.

    Returns None when the bytes are a prefix of a valid netstring.

    Raises InvalidNetstring when the bytes are not a prefix of a valid
    netstring.

    >>> decode(b'5:hello,more')
    ('hello', b'more')

    """

    colon = netstring.find(b':')
    if colon == -1 and len(netstring) >= 10 or colon >= 10:
        # Avoid cases where the incomplete length is already too
        # long or the length is complete but is too long.
        # A minimum ten-digit length will be approximately 1GB or more
        # which is larger than we should need to handle for this API
        raise InvalidNetstring("message length too long")

    if colon == -1:
        # incomplete length, wait for more bytes
        return None

    lengthstring = netstring[0:colon]
    if colon == 0 or not lengthstring.isdigit():
        raise InvalidNetstring("invalid format, malformed message length")

    length = int(lengthstring)
    comma = colon + length + 1
    if len(netstring) <= comma:
        # incomplete message, wait for more bytes
        return None

    if netstring[comma] != 44: # comma
        raise InvalidNetstring("invalid format, missing comma")

    return (netstring[colon + 1 : comma].decode(), netstring[comma+1:])
