
module Game.Toliman.Internal.Lens (
  module Control.Lens,
  use,
  assign, (.=),
  access,
  store, (.*=),
  accessPrism
  ) where

import Data.Monoid as Monoid (First)
import Control.Applicative ((<$>))

import Monad.State (MonadState(..), gets)
import Control.Lens hiding (use, assign, (.=))

import Monad.Ref (MonadRef(..))

use :: MonadState s m => Getting a s a -> m a
use l = gets $ view l

assign :: (MonadState s m, Functor m) => ASetter s s a b -> b -> m ()
assign l x = (& l .~ x) <$> get >>= put

(.=) :: (MonadState s m, Functor m) => ASetter s s a b -> b -> m ()
(.=) = assign

access :: (MonadRef s m) => Getting a s a -> m a
access l = (^. l) <$> getRef

store :: (MonadRef s m) => ASetter s s a b -> b -> m ()
store l x = (& l .~ x) <$> getRef >>= putRef

(.*=) :: (MonadRef s m) => ASetter s s a b -> b -> m ()
(.*=) = store

accessPrism :: (MonadRef s m) => Getting (Monoid.First a) s a -> m (Maybe a)
accessPrism l = (^? l) <$> getRef
