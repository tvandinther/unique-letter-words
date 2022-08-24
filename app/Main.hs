module Main where

import Lib (
    -- WordSet,
    WordsMap,
    WordsMapPair,
    WordDetail,
    noRepeatLetters,
    wordSet,
    attachBitSet,
    mergeAnagrams,
    mergeUniqueSets,
    wordsMapPair,
    )
import Data.Map (Map, toList, union, map)
import Data.List (foldl')
import Data.Maybe (isJust)
import qualified Data.Map as Map
import Control.Parallel.Strategies (parListChunk, rdeepseq, using, parList)
import Data.Time ( getCurrentTime, diffUTCTime )

-- main = putStrLn "Hello World"

main :: IO ()
main = do
    start <- getCurrentTime
    words <- lines <$> readFile "words_alpha.txt"
    let wordDetails = toList $ allWordSets words
    let process = processAllWords wordDetails
    let processInParallel = processAllWords (wordDetails `using` parList rdeepseq)
    let results = iterate processInParallel (Prelude.map wordsMapPair wordDetails `using` parList rdeepseq) !! 4
    print $ length results
    end <- getCurrentTime
    print $ diffUTCTime end start

benchmarkWords :: [String]
benchmarkWords = ["fjord", "gucks", "nymph", "vibex", "waltz"]

processAllWords :: [WordDetail] -> [WordsMapPair] -> [WordsMapPair]
processAllWords iws rws = toList $ foldl' (\acc w -> acc `Map.union` processWordSet w rws) Map.empty iws

-- allWordSets :: [String] -> WordsMap
allWordSets :: [String] -> Map Int [String]
allWordSets ws = mergeAnagrams $ Prelude.map attachBitSet $ filter noRepeatLetters $ filter ((== 5) . length) ws

processWordSet :: WordDetail -> [WordsMapPair] -> WordsMap
processWordSet wd = foldl' (\acc w -> maybe acc (insertWordsMapPairInto acc) $ mergeUniqueSets w wd) Map.empty

insertWordsMapPairInto :: Ord k => Map k a -> (k, a) -> Map k a
insertWordsMapPairInto m (k,v) = Map.insert k v m
