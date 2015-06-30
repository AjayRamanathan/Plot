{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Plots.Types
    ( 
    -- * General plot options
    PlotOpt
    , x
    , y
    , height
    , width
    , title
    , file
    , xlab
    , ylab
    , xNames
    , yNames
    , xLabelOpt
    , yLabelOpt
    , extra
    , pads

    -- * Line Options
    , LinePlotOpt
    , showPoint 

    , PlotFunction
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

import Plots.Axis

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

data PlotOpt datX datY opt = PlotOpt
    { _plotOptX :: [datX]
    , _plotOptY :: [datY]
    , _plotOptHeight :: Double
    , _plotOptWidth :: Double
    , _plotOptXlab :: String
    , _plotOptYlab :: String
    , _plotOptXNames :: [String]
    , _plotOptYNames :: [String]
    , _plotOptXLabelOpt :: LabelOpt
    , _plotOptYLabelOpt :: LabelOpt
    , _plotOptTitle :: String
    , _plotOptFile :: String
    , _plotOptExtra :: opt
    , _plotOptPads :: (Double, Double)  -- (x,y)
    }

makeFields ''PlotOpt

instance Default opt => Default (PlotOpt datX datY opt) where
    def = PlotOpt
        { _plotOptX = []
        , _plotOptY = []
        , _plotOptHeight = 480
        , _plotOptWidth = 480
        , _plotOptXlab = ""
        , _plotOptYlab = ""
        , _plotOptXNames = []
        , _plotOptYNames = []
        , _plotOptXLabelOpt = def
        , _plotOptYLabelOpt = def
        , _plotOptTitle = ""
        , _plotOptFile = "plot.png"
        , _plotOptExtra = def
        , _plotOptPads = (0.1,0.1)
        }

--- remove this
data LinePlotOpt = LinePlotOpt 
    { _linePlotOptShowPoint :: Bool
    }

makeFields ''LinePlotOpt

instance Default LinePlotOpt where
    def = LinePlotOpt
        { _linePlotOptShowPoint = False
        }
