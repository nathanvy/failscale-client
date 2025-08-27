# failscale-client
Like tailscale, except with additional fail.

## wtf

This is a Common Lisp implementation of a subset of the tailscale client, which establishes point-to-point links for a single "failnet" network, analogous to a single tailnet.  It should run on MacOS as well as most BSD and Linux distros, but absolutely no consideration is made for portability to other Lisps, CL implementations, or Windows.

`failscale-client` relies on `sudo -n` working without a password, so your user must exist in sudoers with the appropriate permissions, etc.
