{-# LANGUAGE BangPatterns, MultiParamTypeClasses, FunctionalDependencies #-}
{- |
   module      : Game.Centauri.Core.Map
   copyright   : (c) Duncan Burke
   license     : MPL
   maintaner   : Duncan Burke <duncankburke@gmail.cmo>
-}
module Game.Centauri.Core.Map
       ( Coord,
         GameMap,
         Planar,
         Hex ) where
import Data.Word

type Coord = Word32

class GameMap m t | t -> m where
  neighbours :: m -> t -> [t]
  norm :: m -> t -> t -> Coord

instance GameMap Planar Hex where
  neighbours map (Hex x y) =
    [ Hex (x+a) (y+b) |
      (pred_a, a) <- [ (x /= 0, -1), (True, 0), (x < width map, 1)],
      (pred_b, b) <- [ (y /= 0, -1), (True, 0), (y < height map, 1)],
      pred_a && pred_b]

  norm map h1 h2 = x h1 * x h2 + y h1 * y h2

data Planar = Planar { width :: !Coord,
                       height :: !Coord } deriving (Read, Show)

data Hex = Hex { x :: Coord, y :: Coord } deriving (Read, Show, Eq)
