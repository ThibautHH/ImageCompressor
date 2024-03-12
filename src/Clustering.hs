{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- Clustering
-}

module Clustering (initClusters) where

import Lib (Pixel(color))

import System.Random
import Data.Bifunctor (Bifunctor(first))

data Cluster = Cluster {
    color :: (Int, Int, Int),
    pixels :: [Pixel]
} deriving (Show)

initClusters :: Int -> StdGen -> [Pixel] -> [Cluster] -> ([Cluster], StdGen)
initClusters 0 gen _ ls = (ls, gen)
initClusters k gen ps ls = first (Cluster (Lib.color (ps !! fst rand)) []:) prevClusters
  where
    rand = randomR (0, length ps - 1) gen
    prevClusters = initClusters (k - 1) (snd rand) ps ls
