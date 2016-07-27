tinfoil
=======

```
Feind hört mit!
```

`tinfoil` is a collection of cryptographic building blocks, designed
to provide a unified interface to secure and vetted modules which
reflect Ambiata best practices.

Merging
=======

All changes must be signed off by Mark and Sharif before being merged.

Scope
=====

All core cryptographic functionality required by an Ambiata project
belongs in `tinfoil`. This includes:

 - Credential hashing and verification.
 - Key derivation functions.
 - Key and credential generation.
 - Nonce generation.
 - HMACs and signed requests.
 - Bindings and wrappers around low-level libraries (`scrypt`,
   `libsodium`, et cetera).

Guidelines
==========

 - Simple and secure by default - the obvious solution to a problem
   should also be the correct one.
 - Robust to timing attacks - exposed functions must retain their
   timing characteristics regardless of the context in which they are
   called, and this should be verified with benchmarks.
 - Paranoid - performance is a distant second to correctness.

Random numbers
--------------

 - This package should *never* use `System.Random`, even if you're
   sure it doesn't matter in that particular case. Modules suitable for
   obtaining cryptographically-secure random numbers include:

    - `Crypto.Random` from
      [crypto-api](https://hackage.haskell.org/package/crypto-api).
    - `System.Entropy` from
      [entropy](https://hackage.haskell.org/package/entropy).

Thread safety
-------------

All functions in `tinfoil` are thread-safe unless explicitly noted
otherwise in the function's documentation.

Conceptual warriors
===================

 - Sharif
 - Mark
