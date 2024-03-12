{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- Main
-}

module Main (main) where

import Lib (readPixelFile)
import Conf (Conf(file), confParser)

import System.Exit (exitWith, ExitCode(ExitFailure))

import Options.Applicative (execParser, info, failureCode, (<**>), helper)

main :: IO ()
main = do
  conf <- execParser opts
  pixels <- readPixelFile $ file conf
  _ <- case pixels of
    Nothing -> exitWith $ ExitFailure 84
    Just ps -> print ps
  print conf
  where
    opts = info (confParser <**> helper) $ failureCode 84
