{-# LANGUAGE NamedFieldPuns #-}

import           Data.Maybe (fromMaybe)
import           Graphics.Gloss (Display (InWindow), play, white)
import           Numeric.Natural (Natural)
import           System.Environment (lookupEnv)
import           System.Random (newStdGen)
import           Text.Read (readMaybe)

import           Haskarium.Const (height, width)
import           Haskarium.Draw (draw)
import           Haskarium.Generate (makeCreatures)
import           Haskarium.Motion (onEvent, onTick)
import           Haskarium.Types (World (..))

data Config = Config
    { nAnts :: Natural
    , nCentipedes :: Natural
    , nFleas :: Natural
    , nFlies :: Natural
    }

main :: IO ()
main = do
    Config{nAnts, nCentipedes, nFleas, nFlies} <- getConfig
    g0 <- newStdGen
    let (g1, ants)        = makeCreatures window g0 nAnts
    let (g2, centipedes)  = makeCreatures window g1 nCentipedes
    let (g3, fleas)       = makeCreatures window g2 nFleas
    let (g4, flies)       = makeCreatures window g3 nFlies
    let startWorld = World{ants, centipedes, fleas, flies, randomGen = g4}
    play display white refreshRate startWorld draw onEvent onTick
  where
    window =
        ( (- fromIntegral width / 2, - fromIntegral height / 2)
        , (  fromIntegral width / 2,   fromIntegral height / 2)
        )
    display = InWindow "haskarium" (width, height) (0, 0)
    refreshRate = 60

getConfig :: IO Config
getConfig =
    Config
    <$> getParam "ants" 1
    <*> getParam "centipedes" 1
    <*> getParam "fleas" 1
    <*> getParam "flies" 1
  where
    getParam param def = parseNum def <$> lookupEnv param
    parseNum def = maybe def (fromMaybe def . readMaybe)
