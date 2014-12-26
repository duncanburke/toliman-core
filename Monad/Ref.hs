{-# LANGUAGE UndecidableInstances #-}

module Monad.Ref (
  MonadRef (..)
  ) where

import Data.IORef

import Control.Monad.Lift.IO (MonadIO, liftIO)
import Monad.Reader

class (MonadIO m, Monad m, Functor m) => MonadRef s m | m -> s where
  getRef :: m s
  putRef :: s -> m ()

instance (MonadIO m, MonadReader (IORef a) m, Functor m) => MonadRef a m where
  getRef = ask >>= liftIO . readIORef
  putRef s = do
    ref <- ask
    liftIO $ writeIORef ref s
