# monad-io-adapter [![Build Status](https://travis-ci.org/cjdev/monad-io-adapter.svg?branch=master)](https://travis-ci.org/cjdev/monad-io-adapter)

This Haskell package provides utilities for converting between computations parameterized via two different typeclasses, both of which can be used to abstract over monad transformer stacks with `IO` at the base. Unfortunately, both classes are frequently used in the Haskell ecosystem, since they have minor differences:

  - `MonadIO` comes from the `base` package (as of `base` version 4.9.0.0), and it provides a `liftIO` operation. It is an extremely simple typeclass, focusing exclusively on lifting `IO` actions through transformer stacks with `IO` at the base.

  - `MonadBase` comes from the `transformers-base` package, and it is a generalized version of `MonadIO`. It provides a more general `liftBase` function, which allows lifting to an arbitrary base monad.

    Generally, this additional power isn’t especially useful, but `MonadBase` appears most often through `MonadBaseControl`, a subclass from the `monad-control` package that enables lifting operations that accept an action in negative position. This class has no `IO`-specialized equivalent (not directly, at least), so it often appears in lifted “control” operations.

Due to these typeclasses being unrelated, it’s not entirely uncommon to end up with type signatures like `(MonadIO m, MonadBaseControl IO m) => ...`, which are a little silly, since `MonadBaseControl IO` really includes all the power of `MonadIO`.

For more information, [see the documentation on Hackage][monad-io-adapter].

[monad-io-adapter]: https://hackage.haskell.org/package/monad-io-adapter
