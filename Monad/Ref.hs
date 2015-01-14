{-# LANGUAGE UndecidableInstances #-}

module Monad.Ref (
  MonadRef (..),
  RefT,
  runRefT
  ) where

import Data.IORef (IORef, readIORef, writeIORef, newIORef)

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Lift.IO (MonadIO, liftIO)
import Monad.Reader (MonadReader(..))

class (MonadIO m, Monad m, Functor m) => MonadRef s m | m -> s where
  getRef :: m s
  putRef :: s -> m ()

instance (MonadIO m, MonadReader (IORef a) m, Functor m) => MonadRef a m where
  getRef = ask >>= liftIO . readIORef
  putRef s = do
    ref <- ask
    liftIO $ writeIORef ref s

type RefT s m = ReaderT (IORef s) m

runRefT :: (MonadIO m) => RefT s m a -> s -> m (a, s)
runRefT m s = do
  ref <- liftIO $ newIORef s
  a <- runReaderT m ref
  s' <- liftIO $ readIORef ref
  return (a, s')
