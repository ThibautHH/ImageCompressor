{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- Clustering
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Clustering (initClusters, fillCluster, PrintList(PrintList)) where

import Lib (Pixel(color))
import Data.List (elemIndex, intercalate)
import Data.Maybe (fromJust)

import System.Random
import Data.Bifunctor (Bifunctor(first))

newtype PrintList a = PrintList a deriving (Functor)

data Cluster = Cluster {
    centroid :: (Int, Int, Int),
    pixels :: [Pixel]
} deriving (Eq)

type Clusters = PrintList [Cluster]

type Pixels = PrintList [Pixel]

instance Show Pixels where
    show (PrintList ps) = intercalate "\n" $ map show ps

instance Show Cluster where
    show (Cluster col []) = "--\n" ++ show col ++ "\n-"
    show (Cluster col ps) = "--\n" ++ show col ++ "\n-\n"
        ++ show (PrintList ps)

instance Show Clusters where
    show (PrintList cls) = intercalate "\n" $ map show cls

initClusters :: Int -> StdGen -> [Pixel] -> [Cluster] -> ([Cluster], StdGen)
initClusters 0 gen _ ls = (ls, gen)
initClusters k gen ps ls = first (Cluster (color (ps !! fst rand)) []:)
                           prevClusters
  where
    rand = randomR (0, length ps - 1) gen
    prevClusters = initClusters (k - 1) (snd rand) ps ls


-- remplit les clusters avec les pixels
fillCluster :: [Pixel] -> [Cluster] -> [Cluster]
fillCluster ps clusters = foldl insertIntoCluster clusters ps

-- insère un pixel dans le cluster le plus proche
insertIntoCluster :: [Cluster] -> Pixel -> [Cluster]
insertIntoCluster cls px = cluster{pixels=px : pixels cluster} :
                           filter (/= cluster) cls
  where
    cluster = findCluster cls px

-- renvoie le cluster avec le centroid le plus proche du pixel
findCluster :: [Cluster] -> Pixel -> Cluster
findCluster cls px = cls !! getSmallest (getCentroidDist px cls)

-- renvoie la distance euclidienne entre deux pixels
getEucDist :: (Int, Int, Int) -> (Int, Int, Int) -> Float
getEucDist (x1, y1, z1) (x2, y2, z2) = sqrt $ fromIntegral
    $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2

-- renvoie la liste des distances entre un pixel et les centroids des clusters
getCentroidDist :: Pixel -> [Cluster] -> [Float]
getCentroidDist _ [] = []
getCentroidDist px (c:cls) = getEucDist (color px) (centroid c) :
                             getCentroidDist px cls

-- renvoie l'index de la plus petite distance = l'index du cluster le plus proche
getSmallest :: [Float] -> Int
getSmallest distances = fromJust $ elemIndex (minimum distances) distances
