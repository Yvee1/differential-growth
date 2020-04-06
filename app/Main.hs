module Main where

import           ChaosBox
import           Lib

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

main :: IO ()
main = runChaosBoxWith (\o -> o { optWidth = 10, optHeight = 10, optScale = 60 }) renderSketch

background :: RGB Double -> Generate ()
background = fillScreenRGB

color :: RGB Double -> Render ()
color = setSourceRGB

setup :: Render ()
setup = setLineWidth 0.08

renderSketch :: Generate ()
renderSketch = do
  cairo setup

  (w, h)    <- getSize
  center    <- getCenterPoint

  system    <- start center 0.8 5
  systemRef <- newIORef system

  onMouseDown ButtonLeft $ \_ -> do
    modifyIORefM_ systemRef (const (start center 0.8 5))

  eventLoop $ do
    background black

    system <- modifyIORefM systemRef updateSystem
    cairo $ do
      drawSystem system