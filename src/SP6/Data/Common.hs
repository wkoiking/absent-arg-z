{-# LANGUAGE FlexibleContexts #-}

module SP6.Data.Common where

-- data-default-class
import Data.Default.Class

-- array
import Data.Array.Base
import Data.Array.IArray

safeArray
    :: (IArray a e, Ix i, Bounded i, Default e)
    => [(i, e)]
    -> a i e
safeArray = accumArray (flip const) def (minBound, maxBound)

safeArray2
    :: (IArray a (Maybe e), Ix i, Bounded i)
    => [(i, e)]
    -> a i (Maybe e)
safeArray2 asoc
    = accumArray (flip const) Nothing (minBound, maxBound) $ map (\ (x, y) -> (x, Just y)) asoc

