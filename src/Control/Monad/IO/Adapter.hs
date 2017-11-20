{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
  This module provides utilities for converting between computations
  parameterized via two different typeclasses, both of which can be used to
  abstract over monad transformer stacks with 'IO' at the base. Unfortunately,
  both classes are frequently used in the Haskell ecosystem, since they have
  minor differences:

    * 'MonadIO' comes from the @base@ package (as of @base@ version 4.9.0.0),
      and it provides a 'liftIO' operation. It is an extremely simple typeclass,
      focusing exclusively on lifting 'IO' actions through transformer stacks
      with 'IO' at the base.

    * 'MonadBase' comes from the @transformers-base@ package, and it is a
      generalized version of 'MonadIO'. It provides a more general 'liftBase'
      function, which allows lifting to an arbitrary base monad.

        Generally, this additional power isn’t especially useful, but
        'MonadBase' appears most often through 'MonadBaseControl', a subclass
        from the @monad-control@ package that enables lifting operations that
        accept an action in negative position. This class has no
        'IO'-specialized equivalent (not directly, at least), so it often
        appears in lifted “control” operations.

  Due to these typeclasses being unrelated, it’s not entirely uncommon to end up
  with type signatures like @('MonadIO' m, 'MonadBaseControl' 'IO' m) => ...@,
  which are a little silly, since @'MonadBaseControl' 'IO'@ really includes all
  the power of 'MonadIO'.

  To help alleviate this problem, this module provides a set of utilities for
  converting between the two constraints in situations where possible. The
  'MonadIOAdapterT' transformer implements 'MonadIO' in terms of @'MonadBase'
  'IO'@, and the 'MonadBaseIOAdapterT' transformer implements @'MonadBase' 'IO'@
  in terms of 'MonadIO'.
-}
module Control.Monad.IO.Adapter
  ( -- * Adapting MonadIO in terms of MonadBase IO
    MonadIOAdapterT(MonadIOAdapterT)
  , adaptMonadIO
    -- * Adapting MonadBase IO in terms of MonadIO
  , MonadBaseIOAdapterT(MonadBaseIOAdapterT)
  , adaptMonadBaseIO
  ) where

import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Control
  ( ComposeSt, MonadBaseControl(..), MonadTransControl(..)
  , defaultLiftBaseWith, defaultLiftWith, defaultRestoreM, defaultRestoreT )
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Writer (MonadWriter)


{-|
A transformer that implements 'MonadIO' in terms of @'MonadBase' 'IO'@, simply
converting any calls to 'liftIO' into calls to 'liftBase'. This makes it
possible to discharge 'MonadIO' constraints by converting them into
@'MonadBase' 'IO'@ constraints. For example, given a value with the following
type:

@
foo :: 'MonadIO' m => m ()
@

…it’s possible to convert its type signature using 'adaptMonadIO':

@
'adaptMonadIO' foo :: 'MonadBase' 'IO' m => m ()
@
-}
newtype MonadIOAdapterT m a = MonadIOAdapterT' (IdentityT m a)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadBase b
           , MonadReader r, MonadWriter w, MonadState s, MonadError e
           , MonadThrow, MonadCatch, MonadMask
           , MonadLogger )

instance MonadTransControl MonadIOAdapterT where
  type StT MonadIOAdapterT a = StT IdentityT a
  liftWith = defaultLiftWith MonadIOAdapterT' (\(MonadIOAdapterT' x) -> x)
  {-# INLINE liftWith #-}
  restoreT = defaultRestoreT MonadIOAdapterT'
  {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (MonadIOAdapterT m) where
  type StM (MonadIOAdapterT m) a = ComposeSt MonadIOAdapterT m a
  liftBaseWith = defaultLiftBaseWith
  {-# INLINE liftBaseWith #-}
  restoreM = defaultRestoreM
  {-# INLINE restoreM #-}

instance MonadBase IO m => MonadIO (MonadIOAdapterT m) where
  liftIO = liftBase
  {-# INLINE liftIO #-}

pattern MonadIOAdapterT :: m a -> MonadIOAdapterT m a
pattern MonadIOAdapterT x = MonadIOAdapterT' (IdentityT x)
#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE MonadIOAdapterT #-}
#endif

-- | “Runs” a 'MonadIOAdapterT' computation and returns the remaining
-- transformer stack.
adaptMonadIO :: MonadIOAdapterT m a -> m a
adaptMonadIO (MonadIOAdapterT' (IdentityT x)) = x
{-# INLINE adaptMonadIO #-}


{-|
A transformer that implements 'MonadBase' in terms of 'MonadIO', simply
converting any calls to 'liftBase' into calls to 'liftIO'. This makes it
possible to discharge @'MonadBase' 'IO'@ constraints by converting them into
'MonadIO' constraints. For example, given a value with the following type:

@
foo :: 'MonadBase' 'IO' m => m ()
@

…it’s possible to convert its type signature using 'adaptMonadIO':

@
'adaptMonadBaseIO' foo :: 'MonadIO' m => m ()
@
-}
newtype MonadBaseIOAdapterT m a = MonadBaseIOAdapterT' (IdentityT m a)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadIO
           , MonadReader r, MonadWriter w, MonadState s, MonadError e
           , MonadThrow, MonadCatch, MonadMask
           , MonadLogger )

instance MonadIO m => MonadBase IO (MonadBaseIOAdapterT m) where
  liftBase = liftIO
  {-# INLINE liftBase #-}

pattern MonadBaseIOAdapterT :: m a -> MonadBaseIOAdapterT m a
pattern MonadBaseIOAdapterT x = MonadBaseIOAdapterT' (IdentityT x)
#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE MonadIOAdapterT #-}
#endif

-- | “Runs” a 'MonadBaseIOAdapterT' computation and returns the remaining
-- transformer stack.
adaptMonadBaseIO :: MonadBaseIOAdapterT m a -> m a
adaptMonadBaseIO (MonadBaseIOAdapterT' (IdentityT x)) = x
{-# INLINE adaptMonadBaseIO #-}
