# The `exception-mtl` Package  [![Hackage](https://img.shields.io/hackage/v/exception-mtl.svg)](https://hackage.haskell.org/package/exception-mtl) [![Actions Status: haskell-ci](https://github.com/mainland/exception-mtl/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/mainland/exception-mtl/actions?query=workflow%3Ahaskell-ci)

Provides exception monad transformer instances for the classes defined by [`mtl`](https://hackage.haskell.org/package/mtl).

Import `Control.Monad.Exception.Instances` for its instances:

```haskell
import Control.Monad.Exception.Instances ()
```

The [`exceptions`](https://hackage.haskell.org/package/exceptions) package
supersedes
[`exception-transformers`](https://hackage.haskell.org/package/exception-transformers),
and new code should use it.

## AI assistance

All contributions included in releases through version `0.4.0.2` were made
without AI assistance. AI tools were used in development, review, and release
preparation after that version, through version `0.4.0.3`. The maintainer
remains responsible for this release.
