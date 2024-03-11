{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- Main
-}

module Main (main) where

import Options.Applicative
import Lib (getPixels)
import Conf (Conf(file), confParser)

main :: IO ()
main = do
  conf <- execParser opts
  pixels <- getPixels $ file conf
  print conf
  print pixels
  where
    opts = info (confParser <**> helper) fullDesc
