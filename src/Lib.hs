module Lib
    (
        -- WordSet,
        WordsMap,
        WordsMapPair,
        WordDetail,
        attachBitSet,
        wordSet,
        wordsMapPair,
        mergeUniqueSets,
        noRepeatLetters,
        mergeAnagrams,
    ) where

import Data.Char ( ord, toLower )
import Data.Bits ( Bits((.|.), shiftL, bit, (.&.), zeroBits) )
import Data.Maybe (isJust)
import Data.Map (fromListWith, Map)
import qualified Data.Map as Map

type Alpha = Char
type Word = [Alpha]

type WordsMap = Map Int [[String]]
type WordsMapPair = (Int, [[String]])
type WordDetail = (Int, [String])

data WordSet = WordSet {
    words' :: [[String]],
    bitSet :: Int
} deriving Show

alphaToBitSet :: Bits a => Alpha -> a
alphaToBitSet c = shiftL (bit 0) $ ord (toLower c) - 97

alphaListToBitSet :: (Bits a) => [Alpha] -> a
alphaListToBitSet = foldl (\acc c -> acc .|. alphaToBitSet c) zeroBits

attachBitSet :: (Bits a, Ord a) => [Alpha] -> (a, [Alpha])
attachBitSet word = (alphaListToBitSet word, word)

wordSet :: (Int, [String]) -> WordSet
wordSet (b, ws) = WordSet {
    words' = [ws],
    bitSet = b
}

wordsMapPair :: WordDetail -> WordsMapPair
wordsMapPair (b, ws) = (b, [ws])

mergeAnagrams :: (Bits a, Ord a) => [(a, String)] -> Map a [String]
mergeAnagrams xs = fromListWith (++) [(b, [s]) | (b, s) <- xs]

isUnique :: Bits a => a -> a -> Bool
isUnique a b = (==) zeroBits $ (.&.) a b

mergeUniqueSets :: WordsMapPair -> WordDetail -> Maybe WordsMapPair
mergeUniqueSets (b1, ws1) (b2, ws2)
    | isUnique b1 b2 = Just (b1 .|. b2, ws2 : ws1)
    | otherwise = Nothing

noRepeatLetters :: [Alpha] -> Bool
noRepeatLetters s = isJust $ go s
    where
        go :: [Alpha] -> Maybe Int
        go [] = Just zeroBits
        go (x:xs) = maybeCombineBits =<< go xs
            where
                maybeCombineBits b = if isUnique b charBit then Just $ b .|. charBit else Nothing
                charBit = alphaToBitSet x
