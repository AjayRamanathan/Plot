{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Plots.Types
    ( 
      PlotFunction
    , PointMap(..)
    , PlotData(..)
    , compose
    , flipMap
    ) where

import Control.Lens
import Data.Default
import Data.Maybe

import Diagrams.Prelude
import Diagrams.Backend.Cairo (B)

--import Plots.Axis

--import Plots.Utils
type DiaR2 = Diagram B R2
type PlotFunction = PointMap Double -> PointMap Double -> [DiaR2]

-- mapping between co-ord system
data PointMap a = PointMap
    { runMap :: a -> Maybe a
    , domain :: (a, a)
    }

-- combine two point system
compose :: PointMap a -> PointMap a -> PointMap (a,a)
compose (PointMap m1 (lo,hi)) (PointMap m2 (lo',hi')) = PointMap mapFn dom
  where
    mapFn (x,y) = do x' <- m1 x
                     y' <- m2 y
                     return (x', y')
    dom = ((lo, lo'), (hi, hi'))
{-# INLINE compose #-}

flipMap :: PointMap Double -> PointMap Double
flipMap (PointMap f (lo, hi)) = PointMap mapFn (lo, hi)
  where
    mapFn x = do x' <- f x
                 return (hi' - x' + lo')
    lo' = fromJust.f $ lo
    hi' = fromJust.f $ hi

class PlotData m a where
    getValues :: m a -> [Double]

instance PlotData [] Int where
    getValues = map fromIntegral

instance PlotData [] Double where
    getValues = id

instance PlotData Maybe a where
    getValues _ = [1.0..]

instance PlotData [] String where
    getValues _ = [1.0..]
