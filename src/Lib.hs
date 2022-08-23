module Lib
    (
        WordSet,
        attachBitSet,
        wordSet,
        mergeUniqueSets,
        noRepeatLetters,
        mergeAnagrams,
    ) where

import Data.Char ( ord, toLower )
import Data.Bits ( Bits((.|.), shiftL, bit, (.&.), zeroBits) )
import Data.Maybe (isJust)
import Data.Map (fromListWith, Map)

type Alpha = Char
type Word = [Alpha]

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

mergeAnagrams :: (Bits a, Ord a) => [(a, String)] -> Map a [String]
mergeAnagrams xs = fromListWith (++) [(b, [s]) | (b, s) <- xs]

isUnique :: Bits a => a -> a -> Bool
isUnique a b = (==) zeroBits $ (.&.) a b

mergeUniqueSets :: WordSet -> WordSet -> Maybe WordSet
mergeUniqueSets a b
    | isUnique (bitSet a) (bitSet b) = Just $ WordSet {
        words' = words' a ++ words' b,
        bitSet = bitSet a .|. bitSet b
    }
    | otherwise = Nothing

noRepeatLetters :: [Alpha] -> Bool
noRepeatLetters s = isJust $ go s
    where
        go :: [Alpha] -> Maybe Int
        go [] = Just zeroBits
        go (x:xs) = (\bits -> if isUnique bits charBit then Just $ bits .|. charBit else Nothing) =<< go xs
            where
                charBit = alphaToBitSet x
