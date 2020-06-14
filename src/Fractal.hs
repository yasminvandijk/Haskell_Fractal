module Fractal where

-- convert screen pixel coordinates to complex numbers and compute their fractal value
computeFractal :: (Int, Int) -> [Float]
computeFractal (w, h) = concatMap line [0..(h-1)]
    where line y = map (\x -> pixel x y) [0..(w-1)]
          pixel x y = fractalValue (re x, im y)
          re x = ((f x) / (f w)) * (2 * 2.1) - 2.1
          im y = ((f y) / (f h)) * (2 * 2.1) - 2.1
          f = fromIntegral

fractalValue :: (Float, Float) -> Float
fractalValue (x, y) = iter 0 100 (0, 0) (x, y)

iter :: Int -> Int -> (Float, Float) -> (Float, Float) -> Float
iter i maxIter z c
    = if i == maxIter
      then 1.0
      else if magnitude z > 2
           then (fromIntegral i) / (fromIntegral maxIter)
           else iter (i + 1) maxIter (add (multiply z z) c) c

magnitude :: (Float, Float) -> Float
magnitude (x, y) = sqrt ((x * x) + (y * y))

multiply :: (Float, Float) -> (Float, Float) -> (Float, Float)
multiply (x1, y1) (x2, y2) = (x, y)
    where x = x1 * x2 - y1 * y2
          y = x1 * y2 + x2 * y1

add :: (Float, Float) -> (Float, Float) -> (Float, Float)
add (x1, y1) (x2, y2) = (x, y)
    where x = x1 + x2
          y = y1 + y2