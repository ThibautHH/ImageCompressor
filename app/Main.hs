{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- Main
-}

module Main (main) where

import Lib (readPixelFile)
import Clustering (initClusters, fillCluster, PrintList(PrintList), computeNewCentroids, loop)
import Conf (Conf(number, limit, file), confParser)

import System.Exit (exitWith, ExitCode(ExitFailure))
import System.Random (initStdGen)

import Options.Applicative (execParser, info, failureCode, (<**>), helper)

main :: IO ()
main = do
  conf <- execParser opts
  pixels <- readPixelFile $ file conf
  gen <- initStdGen
  case pixels of
    Just ps -> print $ PrintList (loop ps (computeNewCentroids $
      fillCluster ps $ fst $
      initClusters (number conf) gen ps [])  ((limit conf) + 1) (limit conf))
    Nothing -> exitWith $ ExitFailure 84
  where
    opts = info (confParser <**> helper) $ failureCode 84
