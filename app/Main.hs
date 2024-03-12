{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- Main
-}

module Main (main) where

import Lib (readPixelFile)
import Clustering (initClusters)
import Conf (Conf(number, file), confParser)

import System.Exit (exitWith, ExitCode(ExitFailure))
import System.Random (initStdGen)

import Options.Applicative (execParser, info, failureCode, (<**>), helper)

main :: IO ()
main = do
  conf <- execParser opts
  pixels <- readPixelFile $ file conf
  gen <- initStdGen
  case pixels of
    Just ps -> print $ fst $ initClusters (number conf) gen ps []
    Nothing -> exitWith $ ExitFailure 84
  where
    opts = info (confParser <**> helper) $ failureCode 84
