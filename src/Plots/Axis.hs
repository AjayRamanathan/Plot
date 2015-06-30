{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Plots.Axis
    ( --AxisFunction(..)
    --, LabelOpt

    --, Axis(..)
    --, axisMap
    --, axisLabels
    --, axisDiag
    --, axisLabelOpt
    --, offsetX
    --, offsetY
    --, rotation
    --, size
    --, fontFamily
    
    --, realAxis
    --, indexAxis
    --, emptyAxis
    --, axis
    
    --, tickLen
    --, minorTickLen
    --, labelOpt
    ) where

import Control.Lens (makeLenses, (^.))
import Data.Default

import Diagrams.Prelude hiding (pad, rotation, size)

import Plots.Ticks
import Plots.Utils
import Plots.Types
{-
-- rendering of labels
data LabelOpt = LabelOpt
    { _labelOptOffsetX :: !Double
    , _labelOptOffsetY :: !Double
    , _labelOptRotation :: !Double
    , _labelOptSize :: !Double
    , _labelOptFontFamily :: !String
    } deriving (Show)

-- makeFields ''LabelOpt

instance Default LabelOpt where
    def = LabelOpt
        { _labelOptOffsetX = 0
        , _labelOptOffsetY = 0
        , _labelOptRotation = 0
        , _labelOptSize = 12
        , _labelOptFontFamily  = "helvetica"
        }

data AxisOpt = AxisOpt
    { _nTick :: !Int
    , _nMinorTick :: !Int
    , _tickLen :: !Double
    , _minorTickLen :: !Double
    , _labelOpt :: !LabelOpt
    }

--makeLenses ''AxisOpt

instance Default AxisOpt where
    def = AxisOpt
        { _nTick = 5
        , _nMinorTick = 4
        , _tickLen = 0.1
        , _minorTickLen = 0.05
        , _labelOpt = def
        }

-- axis
data Axis = Axis
    { _axisMap :: !(PointMap Double)
    , _axisDiag :: !DiaR2
    , _axisLabels :: ![((Double, Double), String)]
    , _axisLabelOpt :: !LabelOpt
    }

--makeLenses ''Axis

-- draw axis of length
newtype AxisFunction = AxisFunction { makeAxis :: Double -> Axis }

instance Default AxisFunction where
    def = emptyAxis 

{-
flipAxisFn :: AxisFunction -> AxisFunction
flipAxisFn axisF = AxisFunction $ do (Axis m labels diag) <- makeAxis axisF
                               let (labelP, label) = unzip labels
                                   newLabels = zip labelP $ reverse label
                               return $ Axis (flipMap m) newLabels diag
                               -}


realAxis :: (Double, Double) -> Double -> AxisOpt -> AxisFunction
realAxis r pad' opt = AxisFunction ( \len ->
    let pMap = linearMap (fromRational l, fromRational u) (pad', len-pad')
        (l, u, step) = autoSteps ((opt^._nTick)-1) r
        axis' = lwO 1 $ axis len pad' $ opt & _nTick .~ tickN'
        labels = zip labelP
            $ map ((show :: Float -> String) . fromRational) [l, l+step .. u]
        tickN' = truncate ((u - l) / step) + 1
        labelP = zip (enumFromThenTo pad' (pad'+stepLabel) (len-pad')) $ repeat 0
        stepLabel = (len - 2*pad') / fromIntegral (tickN' - 1)
    in Axis pMap axis' labels (opt^._labelOpt) )

indexAxis :: Int -> [String] -> Double -> AxisOpt -> AxisFunction
indexAxis num labels pad' opt = AxisFunction
    ( \len -> let axis' = axis len pad' $ opt & _nTick .~ num
                                              & _nMinorTick .~ 0
                                              & _minorTickLen .~ 0
                  pMap = linearMap (1, fromIntegral num) (pad', len-pad')
                  labels' = zip labelP labels
                  labelP = zip (enumFromThenTo pad' (pad'+stepLabel) (len-pad')) $ repeat 0
                  stepLabel = (len - 2*pad') / fromIntegral (num - 1)
              in Axis pMap axis' labels' (opt^._labelOpt)
    )

emptyAxis :: AxisFunction
emptyAxis = AxisFunction $ const $ Axis pMap mempty [] def
  where 
    pMap = PointMap (const Nothing) (0, -1)

axis :: Double -> Double -> AxisOpt -> DiaR2
axis len pad opt = l <> translateX pad (majorTicks <> minorTicks)
  where
    l = fromVertices [ 0 ^& 0, len ^& 0 ]
    majorTicks = ticks (len - 2*pad) (opt^._nTick) (opt^._tickLen)
    minorTicks = ticks (len - 2*pad) minorN (opt^._minorTickLen)
    minorN = ((opt^._nMinorTick) + 1) * ((opt^._nTick) - 1) + 1

ticks :: Double -> Int -> Double -> DiaR2
ticks len tickNum tickL = mconcat [ fromVertices [ x ^& 0, x ^& tickL ] | x <- ticksPos ] 
  where
    ticksPos = enumFromThenTo 0 step len
    step = len / (fromIntegral tickNum - 1)
-}
