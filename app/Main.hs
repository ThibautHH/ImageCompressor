module Main (main) where

import Options.Applicative
import System.Environment (getArgs)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

data Flags = Flags
  { number  :: Int
  , limit   :: Float
  , file    :: String } deriving (Show)

flags :: Parser Flags
flags = Flags
      <$> option auto
          ( long "number"
         <> short 'n'
         <> help "number of colors in the final image"
         <> metavar "INT" )
      <*> option auto
          ( long "limit"
         <> short 'l'
         <> help "convergence limit"
         <> metavar "FLOAT" )
      <*> strOption
          ( long "file"
         <> short 'f'
         <> help "path to the file containing the colors of the pixels"
         <> metavar "String" )

main :: IO ()
main = printFlags =<< execParser opts
  where
    opts = info (flags <**> helper)
      ( fullDesc
     <> progDesc "Compress an image using the k-means algorithm"
     <> header "ImageCompressor an image compressing software" )

printFlags :: Flags -> IO ()
printFlags flags = print flags
printFlags _ = putStrLn "Need Args!"


