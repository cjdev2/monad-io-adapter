module Control.Monad.IO.AdapterSpec (spec) where

import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Test.Hspec

import Control.Monad.IO.Adapter

spec :: Spec
spec = do
  describe "MonadIOAdapterT" $
    it "converts a MonadIO constraint into MonadBase IO" $
      (adaptMonadIO (pure () :: forall n. MonadIO n => n ())
          :: forall m. MonadBase IO m => m ())
        `shouldReturn` ()

  describe "MonadBaseIOAdapterT" $
    it "converts a MonadBase IO constraint into MonadIO" $
      (adaptMonadBaseIO (pure () :: forall n. MonadBase IO n => n ())
          :: forall m. MonadIO m => m ())
        `shouldReturn` ()
