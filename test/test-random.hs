{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List.NonEmpty as NE
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import           P

import           Statistics.Distribution
import           Statistics.Distribution.ChiSquared

import           System.Exit
import           System.IO

import           Tinfoil.Random


sample :: Int -> Int -> IO (Vector Int)
sample nv e =
  (histogram . V.concat) <$> replicateM e drawSome
  where
    vals = NE.fromList [0..(nv-1)]

    drawSome = V.replicateM nv (drawOnce vals)

    histogram os = V.create $ do
      freqs <- MV.replicate nv 0
      V.mapM_ (MV.modify freqs (+1)) os
      pure freqs

pearson :: Int -> Vector Int -> Double
pearson e o =
  V.foldl' pearson' 0 o
  where
    pearson' acc x =
      acc + ((((fromIntegral x) - e') ^ (2 :: Int)) / e')

    e' = fromIntegral e

pValue :: Int -> Double -> Double
pValue nValues x2 =
  complCumulative chisq x2
  where
    chisq = chiSquared dof

    dof = nValues -1

main :: IO ()
main =
  let nVal = 100
      expectedCount = 10000
  in do
  counts <- sample nVal expectedCount
  let x2 = pearson expectedCount counts
  let p = pValue nVal x2
  putStrLn $ "Pearson chi-squared test with set size "
          <> show nVal
          <> " and sample count "
          <> show (nVal * expectedCount)
          <> "."
  putStrLn $ "Histogram: " <> show counts
  putStrLn $ "χ^2: " <> show x2
  putStrLn $ "p-value: " <> show p
  when (p < 0.001) $ do
    putStrLn "p < 0.001 - this could be chance, but could also indicate a problem with sampling in Tinfoil.Random. If it's the former, re-running the tests should fix it."
    exitFailure
