{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}

module Game.Toliman.Internal.Sodium (
  module FRP.Sodium,
  sync,
  PushEvent,
  pushEvent,
  SetBehaviour,
  setBehaviour,
  PushBehaviour,
  pushBehaviour,
  MessageBus,
  messageBus,
  getMessages,
  pushMessage,
  Has_event(..),
  Has_push(..),
  Has_behaviour(..),
  Has_set(..)
  ) where

import Data.Functor ((<$>))
import FRP.Sodium hiding (newBehavior, sync)
import qualified FRP.Sodium as Sodium
import Control.Monad.Lift.IO (MonadIO, liftIO)
import qualified Data.Dequeue as Dequeue

import Game.Toliman.Internal.Types

sync :: (MonadIO m) => Sodium.Reactive a -> m a
sync = liftIO . Sodium.sync


data PushEvent a =
  PushEvent {
    _naev_event :: Sodium.Event a,
    _naev_push :: a -> Sodium.Reactive ()}

makeUnderscoreFields ''PushEvent

pushEvent :: Reactive (PushEvent a)
pushEvent = do
  (_naev_event, _naev_push) <- Sodium.newEvent
  return PushEvent {..}


data SetBehaviour a =
  SetBehaviour {
    _nabv_behaviour :: Sodium.Behaviour a,
    _nabv_set :: a -> Sodium.Reactive ()}

makeUnderscoreFields ''SetBehaviour

setBehaviour :: a -> Reactive (SetBehaviour a)
setBehaviour x = do
  (_nabv_behaviour, _nabv_set) <- Sodium.newBehavior x
  return SetBehaviour {..}


data PushBehaviour e s =
  PushBehaviour {
    _napv_behaviour :: Sodium.Behaviour s,
    _napv_event :: e -> Sodium.Reactive ()}

makeUnderscoreFields ''PushBehaviour

pushBehaviour :: (e -> s -> s) -> s  -> Reactive (PushBehaviour e s)
pushBehaviour f z = do
  (e, _napv_event) <- Sodium.newEvent
  rec
    (_napv_behaviour :: Sodium.Behaviour s) <- hold z es
    let es = snapshot f e _napv_behaviour
  return PushBehaviour {..}


data MessageBusEvent a =
  PushMessage a |
  ClearMessages deriving (Show)

newtype MessageBus a = MessageBus (PushBehaviour (MessageBusEvent a) (Dequeue.BankersDequeue a))

messageBus :: Reactive (MessageBus a)
messageBus = MessageBus <$> (pushBehaviour f Dequeue.empty)
  where
    f :: MessageBusEvent a -> Dequeue.BankersDequeue a -> Dequeue.BankersDequeue a
    f (PushMessage m) = flip Dequeue.pushBack m
    f ClearMessages = \_ -> Dequeue.empty

getMessages :: MessageBus a -> Reactive (Dequeue.BankersDequeue a)
getMessages (MessageBus (PushBehaviour {..})) = do
  _napv_event ClearMessages
  sample _napv_behaviour

pushMessage :: MessageBus a -> a -> Reactive ()
pushMessage (MessageBus (PushBehaviour {..})) = _napv_event . PushMessage