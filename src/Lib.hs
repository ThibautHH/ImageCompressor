module Lib (getPixels) where

import Data.Maybe (mapMaybe)

data Pixel = Pixel {
    position :: (Int, Int),
    color :: (Int, Int, Int)
} deriving (Show)

readPixelTuples :: [String] -> Maybe Pixel
readPixelTuples [pos, col] = Just (Pixel (read pos) (read col))
readPixelTuples _ = Nothing

readPixel :: String -> Maybe Pixel
readPixel = readPixelTuples . words

getPixels :: String -> IO [Pixel]
getPixels path = mapMaybe readPixel . lines <$> readFile path
