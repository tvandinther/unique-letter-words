module Main where

import Lib (
    WordSet,
    noRepeatLetters,
    wordSet,
    attachBitSet,
    mergeAnagrams,
    mergeUniqueSets,
    )
import Data.Map (Map, toList)

main :: IO ()
main = print go

-- go :: [WordSet]
go :: [(Int, [String])]
go =  toList $ mergeAnagrams $ map attachBitSet $ filter noRepeatLetters ["tanks", "villa", "moped", "skant"]
