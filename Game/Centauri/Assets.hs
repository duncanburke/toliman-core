module Game.Centauri.Assets
       (module Config,
        AssetState,
        initAssetState,
        finalAssetState,
        AssetLabel,
        getAsset ) where

import Game.Centauri.Assets.Configuration as Config

import System.FilePath
import Data.Serialize as Serialize
import Data.ByteString as B
import Control.Applicative
import Control.Monad.State as State

data AssetState = AssetState {
  cfg :: AssetConfig  } deriving (Show)

type AssetLabel = String

initAssetState :: AssetConfig -> IO AssetState
initAssetState cfg = do
  return AssetState {
    cfg = cfg }

finalAssetState :: AssetState -> IO ()
finalAssetState st = return ()

getAsset :: (Serialize a) => AssetLabel -> StateT AssetState IO a
getAsset lbl = do
  st <- State.get
  lift $ do
    let path = (assetdir $ cfg $ st) </> lbl
    decoded_ <- decode <$> B.readFile path
    case decoded_ of
      Right decoded -> return decoded
      Left err -> fail err
