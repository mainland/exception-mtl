# Changelog

## 0.4.0.3

- Define `state` in the `MonadState` instance by lifting the underlying monad's
  `state`, instead of using the default built from `get` and `put`.
- Add a test suite. It checks the instances through `ExceptionT`, including
  exception propagation, and checks that `state` calls the underlying monad's
  `state`.
- Document the module and how to import its instances, and note that the
  `exceptions` package supersedes `exception-transformers`.
- Drop GHC 7.x and add GHC 9.8, 9.10, 9.12, and 9.14. Test with the latest
  patch release of each GHC series from 8.0 through 9.14.
- Require `base >= 4.9`, matching the supported GHC 8.0+ range. Raise the lower
  bounds on `exception-transformers`, `mtl`, and `transformers` to 0.4.0.4,
  2.2.1, and 0.5, the oldest versions installable with that `base`.
- Use HTTPS for the source repository and include the README and changelog in
  source distributions.
- Update the maintainer address in the module header and link the package
  homepage.
