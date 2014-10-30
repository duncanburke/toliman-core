{- |
   module      : Game.Centauri.Core.State
   copyright   : (c) Duncan Burke
   license     : MPL
   maintaner   : Duncan Burke <duncankburke@gmail.cmo>
-}
module Game.Centauri.Core.State
       ( GameState,
         initGameState,
         finalGameState ) where

data GameState = GameState { }

initGameState :: IO GameState
initGameState = return $ GameState {}

finalGameState :: GameState -> IO ()
finalGameState st = return ()

