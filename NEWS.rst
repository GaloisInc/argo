News
----

There has not yet been a release. Please add new features and
externally-visible changes to this list so that we can write release
notes as releases occur, and so that clients of the intermediate
development artifacts can keep apprised of ongoing updates.

Recent Changes
==============

- Added a ``doc`` subcommand that causes documentation to be dumped
  about the server.

- Added a ``--public`` flag that causes the socket server to listen for
  external connections

- Added an HTTP interface.

- Replaced the default command line interface with a subcommand-based
  interface. Now, use the ``stdio``, ``socket``, and ``http``
  subcommands to launch the server in each respective mode. Other
  command-line options are unchanged, but validation of nonsensical
  combinations is also stricter than it was before.

- Removed ``--public`` and replaced it with ``--host``. Use ``--host
  ::`` or ``--host 0.0.0.0`` to get the previous behavior of
  ``--public``.

- Added a ``--log`` option that controls debug logging, which is now
  off by default.
