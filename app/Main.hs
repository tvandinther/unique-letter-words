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
import Data.List (foldl', nub)
import Data.Maybe (isJust)

main :: IO ()
main = do
    words <- lines <$> readFile "words_alpha.txt"
    let wordSets = allWordSets benchmarkWords
    let results = iterate (processAllWordSets wordSets) wordSets !! 4
    print results

benchmarkWords :: [String]
benchmarkWords = ["fjord", "gucks", "nymph", "vibex", "waltz"]

processAllWordSets :: [WordSet] -> [WordSet] -> [WordSet]
processAllWordSets iws rws = foldl' (\acc w1 -> acc ++ processWordSet w1 rws) [] iws

allWordSets :: [String] -> [WordSet]
allWordSets ws = map wordSet $ toList $ mergeAnagrams $ map attachBitSet $ filter noRepeatLetters $ filter (\w -> length w == 5) ws

processWordSet :: WordSet -> [WordSet] -> [WordSet]
processWordSet w1 = foldl' (\acc w2 -> maybe acc (: acc) $ mergeUniqueSets w1 w2) []
