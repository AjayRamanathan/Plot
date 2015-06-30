{-# LANGUAGE TemplateHaskell #-}

module Plots.Ticks
    ( ticks
    ) where

import Control.Lens hiding ((#))
import Data.Default
import Data.Maybe

import Diagrams.Prelude

import Plots.PlotArea

data TickOpts = TickOpts
    { _tickLength :: Double
    }

instance Default TickOpts where
    def = TickOpts 
        { _tickLength = 0.1
        }

makeLenses ''TickOpts

ticks :: (PlotData m1 a1, PlotData m2 a2) => m1 a1 -> m2 a2 -> TickOpts -> PlotFunction
ticks xs ys opt mapX mapY = [ticksX, ticksY]
  where
    ticksX = mconcat [ fromVertices [x ^& 0, x ^& (opt^.tickLength)] | x <- mapMaybe (runMap mapX) xs' ]
    ticksY = mconcat [ fromVertices [0 ^& y, (opt^.tickLength) ^& y] | y <- mapMaybe (runMap mapY) ys' ]
    xs' = getValues xs
    ys' = getValues ys
