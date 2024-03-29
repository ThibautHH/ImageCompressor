{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- Lib
-}

module Lib (Pixel(Pixel, position, color), readPixelFile) where

import Text.Read (readMaybe)

data Pixel = Pixel {
    position :: (Int, Int),
    color :: (Int, Int, Int)
} deriving (Eq)

instance Show Pixel where
    show (Pixel pos col) = show pos ++ " " ++ show col

readPixelTuples :: [String] -> Maybe Pixel
readPixelTuples [pos, col] = Pixel <$> readMaybe pos <*> readMaybe col
readPixelTuples _ = Nothing

readPixel :: String -> Maybe Pixel
readPixel = readPixelTuples . words

readPixelFile :: String -> IO (Maybe [Pixel])
readPixelFile path = mapM readPixel . lines <$> readFile path
