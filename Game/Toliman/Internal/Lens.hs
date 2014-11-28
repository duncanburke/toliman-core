
module Game.Toliman.Internal.Lens (
  module Control.Lens,
  use ) where

import Monad.State
import Monad.Reader
import Control.Monad.Lift.IO

import Control.Lens hiding (use)


use :: MonadState s m => Getting a s a -> m a
use l = gets $ view l
