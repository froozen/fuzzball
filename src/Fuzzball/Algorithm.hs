{-|
Module      : Fuzzball.Algorithm
Description : Implementation of a fuzzy matching algorithm

The Fuzzball.Algorithm module implements the actual fuzzy matching algorithm.
-}

module Fuzzball.Algorithm
    ( fuzzyMatch
    , MatchRating(..)
    ) where

-- | The rating of the quality of a fuzzy match
data MatchRating = MatchRating {
      rating  :: Int
    , indexes :: [Int]
    } deriving (Eq, Ord, Show)

-- | Fuzzily match a pattern over a string
fuzzyMatch :: String -> String -> Maybe MatchRating
fuzzyMatch [] _ = Just . MatchRating 0 $ []
fuzzyMatch _ [] = Nothing
fuzzyMatch (p:ps) (c:cs)
    | p == c = fmap found . fmap push . fuzzyMatch ps $ cs
    | otherwise = fmap push . fuzzyMatch (p:ps) $ cs
    where -- Increment the indices
          push mr@(MatchRating _ indexes) =
            let indexes' = map (+1) indexes
            in  mr { indexes = indexes' }

          -- Update the rating and add a new index
          found mr@(MatchRating rating indexes) =
            let rating' = rating + if null indexes then 0 else head indexes
            in mr { rating = rating', indexes = 0:indexes }
