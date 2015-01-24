
module Game.Toliman.Internal.Lens (
  module Control.Lens,
  use,
  assign, (.=),
  (%=),
  access,
  store, (.*=),
  (%*=),
  accessPrism
  ) where

import Data.Monoid as Monoid (First)
import Data.Functor ((<$>))

import Monad.State as State (MonadState(..), gets, modify)
import Control.Lens hiding (use, assign, (.=), (%=))

import Monad.Ref (MonadRef(..), modifyRef)

use :: MonadState s m => Getting a s a -> m a
use l = gets $ view l

assign :: (MonadState s m, Functor m) => ASetter s s a b -> b -> m ()
assign l b = State.modify (l .~ b)

(.=) :: (MonadState s m, Functor m) => ASetter s s a b -> b -> m ()
(.=) = assign

(%=) :: (Profunctor p, MonadState s m) => Setting p s s a b -> p a b -> m ()
l %= f = State.modify (l %~ f)

access :: (MonadRef s m) => Getting a s a -> m a
access l = (^. l) <$> getRef

store :: (MonadRef s m) => ASetter s s a b -> b -> m ()
store l b = modifyRef (l .~ b)

(.*=) :: (MonadRef s m) => ASetter s s a b -> b -> m ()
(.*=) = store

(%*=) :: (Profunctor p, MonadRef s m) => Setting p s s a b -> p a b -> m ()
l %*= f = modifyRef (l %~ f)

accessPrism :: (MonadRef s m) => Getting (Monoid.First a) s a -> m (Maybe a)
accessPrism l = (^? l) <$> getRef
