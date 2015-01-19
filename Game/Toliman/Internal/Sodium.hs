{-# LANGUAGE TemplateHaskell #-}

module Game.Toliman.Internal.Sodium (
  module FRP.Sodium,
  SodiumEvent,
  SodiumBehaviour,
  Has_event(..),
  Has_push(..),
  Has_behaviour(..),
  newEvent,
  newBehaviour,
  sample,
  sync
  ) where

import FRP.Sodium hiding
  (newEvent, newBehaviour, newBehavior,
   sample, sync)
import qualified FRP.Sodium as Sodium
import Control.Monad.Lift.IO (MonadIO, liftIO)

import Game.Toliman.Internal.Types

data SodiumEvent a =
  SodiumEvent {
    _naev_event :: Sodium.Event a,
    _naev_push :: a -> Sodium.Reactive () }

makeUnderscoreFields ''SodiumEvent

-- sample :: Behavior a -> Reactive a

data SodiumBehaviour a =
  SodiumBehaviour {
    _nabv_behaviour :: Sodium.Behaviour a,
    _nabv_push :: a -> Sodium.Reactive () }

makeUnderscoreFields ''SodiumBehaviour

newEvent :: Reactive (SodiumEvent a)
newEvent = do
  (_naev_event, _naev_push) <- Sodium.newEvent
  return SodiumEvent {..}

newBehaviour :: a -> Reactive (SodiumBehaviour a)
newBehaviour x = do
  (_nabv_behaviour, _nabv_push) <- Sodium.newBehavior x
  return SodiumBehaviour {..}

sample :: SodiumBehaviour a -> Reactive a
sample SodiumBehaviour {..} = Sodium.sample _nabv_behaviour

sync :: (MonadIO m) => Sodium.Reactive a -> m a
sync = liftIO . Sodium.sync
