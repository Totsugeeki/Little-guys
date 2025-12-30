module Assets where

import Graphics.Gloss

data Assets = Assets
  { backgroundAssets :: BackgroundAssets
  } deriving (Show)

  data BackgroundAssets = BackgroundAssets
  { gameBackground :: Maybe Picture
  } deriving (Show)