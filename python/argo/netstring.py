"""Argo uses D. J. Berstein's `netstrings <https://cr.yp.to/proto/netstrings.txt>`_
as a lightweight transport layer for JSON RPC.
"""

from typing import Tuple

def encode(string : str) -> bytes:
    """Encode a ``str`` into a netstring.

    >>> encode("hello")
    b'5:hello,'
    """
    bytestring = string.encode()
    return str(len(bytestring)).encode() + b':' + bytestring + b','

def decode(netstring : bytes) -> Tuple[str, bytes]:
    """Decode the first valid netstring from a bytestring, returning its
    string contents and the remainder of the bytestring.

    >>> decode(b'5:hello,more')
    ('hello', b'more')

    """

    i = 0
    length_bytes = bytearray(b'')
    while chr(netstring[i]).isdigit():
        length_bytes.append(netstring[i])
        i += 1
    if chr(netstring[i]).encode() != b':':
        raise ValueError("Malformed netstring, missing :")
    length = int(length_bytes.decode())
    i += 1
    out = bytearray(b'')
    for j in range(0, length):
        out.append(netstring[i])
        i += 1
    if chr(netstring[i]).encode() != b',':
        raise ValueError("Malformed netstring, missing ,")
    i += 1
    return (out.decode(), netstring[i:])
