module Game.Centauri.Assets.Configuration
       (AssetConfig(..),
        initAssetConfig) where

data AssetConfig = AssetConfig {
  assetdir :: FilePath } deriving (Show)

initAssetConfig :: AssetConfig
initAssetConfig = AssetConfig {
  assetdir = "/home/duncan/dev/opencentauri/assets"}
