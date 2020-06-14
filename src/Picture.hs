module Picture where

import Data.ByteString (ByteString, pack)
import Data.Word (Word8)
import Graphics.Gloss

import Fractal

-- create a picture by converting the fractal values to a bitmap
createPicture :: (Int, Int) -> Picture
createPicture (width, height)
              = bitmapOfByteString width
                                   height
                                   bitmapFormat
                                   bitmapData
                                   cacheData
              where bitmapFormat = BitmapFormat TopToBottom PxRGBA
                    bitmapData   = pictureBytes (computeFractal (width, height))
                    cacheData    = False

-- convert the fractal values to a bytestring
pictureBytes :: [Float] -> ByteString
pictureBytes = pack . concatMap colorToByte . map floatToColor

-- convert a float value to a Color
floatToColor :: Float -> Color
floatToColor f = makeColor (sqrt f) f (1 - sqrt f) 1.0

-- convert a Color to bytes
colorToByte :: Color -> [Word8]
colorToByte = f . rgbaOfColor
            where f (r, g, b, a) = map floatToByte [r, g, b, a]

-- convert a float value to a byte
floatToByte :: Float -> Word8
floatToByte f = fromIntegral $ round $ f * 255