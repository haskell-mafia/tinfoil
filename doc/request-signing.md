# Request signing

## Canonical request form

The request is first transformed into a canonical form for signing,
which is constructed as follows (in pseudo-Haskell):

```haskell
r_canonical = intercalate "\n" [
    requestMethod
  , canonicalURI
  , canonicalQueryString
  , canonicalHeaders
  , signedHeaders
  , hashedPayload
  ]
  where
    hashedPayload = hexDigest $ sha256 requestPayload
```

FIXME: finish this section/specify the rest properly

## Signed data

The following data is included in the request's signature:

 - algorithm
 - request date
 - request scope
 - the SHA256 hash of the canonical request

### Request scope

TBD

## Signing key

The signing key is derived from the secret key via an iterated chain
of SHA256 hashes, as follows:

```haskell
let dateKey = hmacSHA256 ("tinfoil-begin" + k) date
    scopeKey = hmacSHA256 dateKey requestScope
    signingKey = hmacSHA256 scopeKey "tinfoil-end"
```

