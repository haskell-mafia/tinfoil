# tinfoil

```
Feind hört mit!
```

`tinfoil` is a collection of cryptographic building blocks, designed
to provide a unified interface to secure and vetted modules which
reflect Ambiata best practices.

# Merging

All changes must be signed off by Mark and Sharif before being merged.

# Scope

All core cryptographic functionality required by an Ambiata project
belongs in `tinfoil`. This includes:

 - Credential hashing and verification.
 - Key derivation functions.
 - HMACs and signed requests.
 - Bindings and wrappers around low-level libraries (`scrypt`,
   `libsodium`, et cetera).

# Guidelines

 - Simple and secure by default - the obvious solution to a problem
   should also be the correct one.
 - Robust to timing attacks - exposed functions must retain their
   timing characteristics regardless of the context in which they are
   called, and this should be verified with benchmarks.
 - Paranoid - performance is a distant second to correctness.
