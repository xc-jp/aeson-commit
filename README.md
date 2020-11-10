# aeson-commit
[![Hackage version](https://img.shields.io/hackage/v/aeson-commit.svg?label=Hackage)](https://hackage.haskell.org/package/aeson-commit)
[![Stackage version](https://www.stackage.org/package/aeson-commit/badge/nightly?label=Stackage)](https://www.stackage.org/package/aeson-commit)
[![Build status](https://img.shields.io/travis/xc-jp/aeson-commit/master.svg?label=Build)](https://travis-ci.org/xc-jp/aeson-commit)

Commitment mechanism for `aeson` parsers.
Commitment means that if some initial parsing succeeds, subsequent failures are unrecoverable.

See [haddocks](https://hackage.haskell.org/package/aeson-commit/docs/Data-Aeson-Commit.html) for more information and examples.
