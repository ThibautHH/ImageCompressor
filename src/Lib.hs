module Lib (Pixel(Pixel, position, color), readPixelFile) where

import Text.Read (readMaybe)

data Pixel = Pixel {
    position :: (Int, Int),
    color :: (Int, Int, Int)
} deriving (Show)

readPixelTuples :: [String] -> Maybe Pixel
readPixelTuples [pos, col] = Pixel <$> readMaybe pos <*> readMaybe col
readPixelTuples _ = Nothing

readPixel :: String -> Maybe Pixel
readPixel = readPixelTuples . words

readPixelFile :: String -> IO (Maybe [Pixel])
readPixelFile path = mapM readPixel . lines <$> readFile path
