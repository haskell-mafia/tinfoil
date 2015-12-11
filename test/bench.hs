{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Criterion.Main

import           Tinfoil.Random

import           P

import           System.IO

main :: IO ()
main = defaultMain [
  bgroup "randomCredential" $ 
    [ bench "1" $ nfIO (randomCredential [] 1)
    , bench "1000" $ nfIO (randomCredential [] 1000)
    , bench "100000" $ nfIO (randomCredential [] 100000)
    ]
  ]
