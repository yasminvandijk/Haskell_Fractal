module Main where

import Graphics.Gloss

-- constants
title :: String
title = "fractal"

windowSize :: (Int, Int)
windowSize = (1000, 1000)

windowPosition :: (Int, Int)
windowPosition = (100, 100)

backgroundColor :: Color
backgroundColor = black

-- display an empty window
main :: IO ()
main = display (InWindow title windowSize windowPosition) 
                backgroundColor 
                Blank