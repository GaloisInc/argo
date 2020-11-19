=================
Protocol Overview
=================

This repository contains servers that implement APIs for interacting
remotely with SAW and Cryptol. These APIs are realized as JSON-RPC_
protocols, and they can be used either over stdio or a socket.

.. _JSON-RPC: https://www.jsonrpc.org/specification

Both protocols are built on a common foundation. The specific actions
available for SAW or Cryptol are, however, different. This document is
divided into three main sections: one about the common protocol, one
about the Cryptol-specific interaction commands, and one about the
SAW-specific commands.

Overall Protocol Structure
==========================

The interaction protocols are designed primarily to support
interactive, rather than batch mode, use of SAW. SAW is a stateful
system: LLVM, JVM, and Cryptol modules are loaded, and then proofs are
carried out in the resulting context. Each step can consume
significant time and computing resources, so it is best to avoid
needless repetition of steps. Additionally, a step might fail, and
recovering from that failure may require backtracking an arbitrary
number of steps.

To minimize the necessary amount of recomputation when backtracking
and exploring alternative proof strategies, the server performs
extensive caching. When a client issues a command, they additionally
provide a representation of the state in which the command is to be
performed; when the server has performed the command, it returns a new
state along with the result. Clients may re-use server states as many
times as they desire.

Explicitly representing the state in the protocol has additional
benefits. The server can be restarted at any time, with the worst
consequence being a delay in its response. The client can maintain its
own handle into the server state, persist it to disk, and pick up
where it left off on another computer.


Transport
---------

The JSON-RPC messages in the protocol are encoded in netstrings_, to
make it easier to allocate buffers ahead of time and to avoid bugs
with missing terminator characters.

.. _netstrings: http://cr.yp.to/proto/netstrings.txt

The servers support two modes of operation: stdio and sockets.

State
-----

According to the JSON-RPC specification, the ``params`` field in a
message object must be an array or object. In this protocol, it is
always an object. While each message may specify its own arguments,
every message has a parameter field named ``state``.

When the first message is sent from the client to the server, the
``state`` parameter should be initialized to the JSON null value
``null``. Replies from the server may contain a new state that should
be used in subsequent requests, so that state changes executed by the
request are visible. Prior versions of this protocol represented the
initial state as the empty array ``[]``, but this is now deprecated
and will be removed.

In particular, per JSON-RPC, non-error replies are always a JSON
object that contains a ``result`` field. The result field always
contains an ``answer`` field and a ``state`` field, as well as
``stdout`` and ``stderr``.

``answer``
  The value returned as a response to the request (the precise
  contents depend on which request was sent)

``state``
  The state, to be sent in subsequent requests. If the server did not
  modify its state in response to the command, then this state may be
  the same as the one sent by the client.

``stdout`` and ``stderr``
  These fields contain the contents of the Unix ``stdout`` and
  ``stderr`` file descriptors. They are intended as a stopgap measure
  for clients who are still in the process of obtaining structured
  information from the libraries on which they depend, so that
  information is not completely lost to users. However, the server may
  or may not cache this information and resend it. Applications are
  encouraged to used structured data and send it deliberately as the answer.

The precise structure of states is considered an implementation detail
that could change at any time. Please treat them as opaque tokens that
may be saved and re-used within a given server process, but not
created by the client directly.
