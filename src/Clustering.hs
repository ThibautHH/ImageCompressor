{-
-- EPITECH PROJECT, 2024
-- ImageCompressor
-- File description:
-- Clustering
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE NamedFieldPuns #-}

module Clustering (initClusters, loop, PrintList(PrintList)) where

import Lib (Pixel(..))
import Data.List (intercalate)

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
    newPs = take (fst rand) ps ++ drop (fst rand + 1) ps
    prevClusters = initClusters (k - 1) (snd rand) newPs ls


-- boucle jusqu'à ce que les centroids bougent moins que la tolérance
--     liste ori   curr cls     diff     limit    new cls
loop :: [Pixel] -> [Cluster] -> Float -> Float -> [Cluster]
loop pxs cls diff l
    | diff < l = cls
    | otherwise = loop pxs (map fst clusters) (maximum $ map snd clusters) l
    where
        clusters = map updateCentroid $ fillCluster pxs $ emptyClusters cls

tupleAdd :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
tupleAdd (a, b, c) (d, e, f) = (a + d, b + e, c + f)

-- calcule la moyenne des pixels d'un cluster
computeAverage :: [Pixel] -> (Int, Int, Int)
computeAverage pxs = (nr `div` len, ng `div` len, nb `div` len)
    where
        (nr, ng, nb) = foldr (tupleAdd . color) (0, 0, 0) pxs
        len = length pxs

-- calcule le nouveau centroid de chaque cluster
updateCentroid :: Cluster -> (Cluster, Float)
updateCentroid Cluster{centroid=c, pixels} = (new, dist (centroid new) c)
    where new = Cluster (computeAverage pixels) pixels

-- vide les clusters puis les remplis
emptyClusters :: [Cluster] -> [Cluster]
emptyClusters = map (\Cluster{centroid} -> Cluster centroid [])

-- remplit les clusters avec les pixels
fillCluster :: [Pixel] -> [Cluster] -> [Cluster]
fillCluster ps clusters = foldl insertIntoCluster clusters ps

-- insère un pixel dans le cluster le plus proche
insertIntoCluster :: [Cluster] -> Pixel -> [Cluster]
insertIntoCluster cls px = cluster{pixels=px : pixels cluster} :
                           filter (/= cluster) cls
  where
    cluster = findCluster px cls

minDistance :: Ord b => [(a, b)] -> a
minDistance = fst . foldl1 (\a@(_, x) b@(_, y) -> if y < x then b else a)

-- renvoie le cluster avec le centroid le plus proche du pixel
findCluster :: Pixel -> [Cluster] -> Cluster
findCluster px cls = minDistance . zip cls $ distances
    where distances = map (dist (color px) . centroid) cls

-- renvoie la distance euclidienne entre deux pixels
dist :: (Int, Int, Int) -> (Int, Int, Int) -> Float
dist (x1, y1, z1) (x2, y2, z2) = sqrt $ fromIntegral
    $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2
