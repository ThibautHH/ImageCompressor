module Conf (Conf(Conf, number, limit, file), confParser) where

import Options.Applicative

data Conf = Conf {
    number :: Int,
    limit :: Float,
    file :: String
} deriving (Show)

clusterCountOption :: Mod OptionFields a
clusterCountOption = long "number"
                    <> short 'n'
                    <> help "Number of colors in the compressed image; the cluster count in the k-means algorithm"
                    <> metavar "INT"

convergenceLimitOption :: Mod OptionFields a
convergenceLimitOption = long "limit"
                        <> short 'l'
                        <> help "Convergence limit"
                        <> metavar "FLOAT"

pathOption :: Mod OptionFields a
pathOption = long "file"
            <> short 'f'
            <> help "File containing the colors of the pixels"
            <> metavar "STRING"

confParser :: Parser Conf
confParser = Conf
            <$> option auto clusterCountOption
            <*> option auto convergenceLimitOption
            <*> strOption pathOption
