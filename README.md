# hkdf

[![](https://img.shields.io/hackage/v/ppad-hkdf?color=blue)](https://hackage.haskell.org/package/ppad-hkdf)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-hkdf-lightblue)](https://docs.ppad.tech/hkdf)

A HMAC-based key derivation function (HKDF) per
[RFC5869](https://datatracker.ietf.org/doc/html/rfc5869).

## Usage

A sample GHCi session:

```
  > :set -XOverloadedStrings
  > -- import qualified
  > import qualified Crypto.KDF.HMAC as KDF
  >
  > -- supply your own HMAC function
  > import qualified Crypto.Hash.SHA256 as SHA256
  >
  > -- derive a 32-byte key from a secret
  > Just (KDF.derive SHA256.hmac "my salt" "my optional info" 32 "my secret input")
  "\EM\232\v\140\202\230\f2:\221n\221\209\233\US\209>\174_!\138\255\\C\150\237^X\226\tt\252"
```

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/hkdf][hadoc].

## Performance

The aim is best-in-class performance for pure, highly-auditable Haskell
code.

Current benchmark figures on an M4 Silicon MacBook Air look like (use
`cabal bench` to run the benchmark suite):

```
  benchmarking ppad-hkdf/HKDF-SHA256/derive (outlen 32)
  time                 1.549 μs   (1.547 μs .. 1.553 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 1.565 μs   (1.560 μs .. 1.572 μs)
  std dev              20.80 ns   (18.40 ns .. 24.68 ns)
  variance introduced by outliers: 12% (moderately inflated)

  benchmarking ppad-hkdf/HKDF-SHA512/derive (outlen 32)
  time                 2.089 μs   (2.088 μs .. 2.090 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 2.090 μs   (2.088 μs .. 2.092 μs)
  std dev              5.637 ns   (3.965 ns .. 8.886 ns)
```

## Security

This library aims at the maximum security achievable in a
garbage-collected language under an optimizing compiler such as GHC, in
which strict constant-timeness can be [challenging to achieve][const].

The HKDF implementation within has been tested against the [Project
Wycheproof vectors][wyche] available for SHA-256 and SHA-512, using
the HMAC functions from [ppad-sha256][sh256] and [ppad-sha512][sh512]
respectively.

If you discover any vulnerabilities, please disclose them via
security@ppad.tech.

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter a
development shell with:

```
$ nix develop
```

Then do e.g.:

```
$ cabal repl ppad-hkdf
```

to get a REPL for the main library.

[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
[hadoc]: https://docs.ppad.tech/hkdf
[sh256]: https://git.ppad.tech/sha256
[sh512]: https://git.ppad.tech/sha512
[const]: https://www.chosenplaintext.ca/articles/beginners-guide-constant-time-cryptography.html
[wyche]: https://github.com/C2SP/wycheproof
