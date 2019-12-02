# Java client (currently in stasis)

This directory is a partial implementation of a java client for the argo
protocol and cryptol/SAW servers thereof. It is currently **in stasis** and is
not being maintained: no guarantees are made about its compliance with the
current implementation, or its development proceeding in lockstep with the work
on the protocol, servers, or other clients. The code is parked here for the aid
of potential future implementors who might wish to revive it.

# Building the java client

```
$ gradle build
```

# Running the client

```
$ gradle run --console=plain --args='<PATH_TO_SERVER> <DESIRED STARTING DIRECTORY>'
```
