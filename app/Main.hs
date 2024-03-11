module Main (main) where

import Options.Applicative
import Conf (confParser)

main :: IO ()
main = do
  conf <- execParser opts
  print conf
  where
    opts = info (confParser <**> helper) fullDesc
