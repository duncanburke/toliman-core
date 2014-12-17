
module Game.Toliman.Internal.Lens (
  module Control.Lens,
  use, (^=.),
  assign, (.=),
  use_ref, (^*.),
  assign_ref, (.*)
  ) where

import Monad.State
import Monad.Reader
import Data.IORef
import Control.Monad.Lift.IO

import Control.Lens hiding (use, assign, (.=))


use :: MonadState s m => Getting a s a -> m a
use l = gets $ view l

(^=.) :: MonadState s m => Getting a s a -> m a
(^=.) = use

assign :: MonadState s m => ASetter s s a b -> b -> m ()
assign l x = do
  s <- get
  put $ s & l .~ x

(.=) :: MonadState s m => ASetter s s a b -> b -> m ()
(.=) = assign

use_ref :: (MonadReader (IORef s) m, MonadIO m) => Getting a s a -> m a
use_ref l = do
  s <- ask >>= liftIO . readIORef
  return $ s ^. l

(^*.) :: (MonadReader (IORef s) m, MonadIO m) => Getting a s a -> m a
(^*.) = use_ref

assign_ref :: (MonadReader (IORef s) m, MonadIO m) => ASetter s s a b -> b -> m ()
assign_ref l x = do
  ref <- ask
  s <- liftIO $ readIORef ref
  liftIO $ writeIORef ref $ s & l .~ x

(.*) :: (MonadReader (IORef s) m, MonadIO m) => ASetter s s a b -> b -> m ()
(.*) = assign_ref
