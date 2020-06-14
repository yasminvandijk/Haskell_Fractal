module Fractal where

-- TODO
createFractal :: (Int, Int) -> [Float]
createFractal (width, height) = (take (width * height) (repeat 1.0))