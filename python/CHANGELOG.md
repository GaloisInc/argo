# argo-client v0.0.8 (27 Aug 2021)
+ Improvements related to notification/request interference
  (i.e., when a single client sends a notification immediately
   followed by a request).

# argo-client v0.0.7 (25 Aug 2021)
+ Change the behavior of the `Command` `state` method so that after a `Command`
  raises an exception, subsequent interactions will not also raise the same
  exception.

# argo-client v0.0.6 (22 Jul 2021)
+ Add logging option to client. See `ServerProcess.logging(..)` and
  `ServerConnection.logging(..)`.

# argo-client v0.0.5 (23 Jun 2021)
+ Add HTTP flag for TLS cert verification options.

# argo-client v0.0.4 (2 Mar 2021)
+ Add support for notifications.
+ Improved error handling.

# argo-client v0.0.3 (9 Feb 2021)
+ Add mypy annotations to package for argo_client module.

# argo-client v0.0.2 (9 Feb 2021)
+ Initial release.
