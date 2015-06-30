{-# LANGUAGE TemplateHaskell #-}

module Plots.Geom.Step
    ( Geom_Step(..)

    , transformstep
    , drawstepS
    , drawstepM'
    , createstep

    , stepplot'
    , stepplot
    , stepplotgroup
    ) where

import Control.Lens (makeLenses, (^.))
import Data.Default
import Data.Maybe
import Data.List

import Diagrams.Prelude

import Plots.Types
import Plots.Utils

-----------------------------------------------

data Geom_Step = Geom_Step
    { stepX :: [Double]
    , stepY :: [Double]
    , stepSize :: Double
    , stepColour :: Colour Double
    , stepAlpha :: Double
  --, stepLinetype ::
    , stepCap :: LineCap
    , stepJoin :: LineJoin
    }

instance Default Geom_Step where
  def = Geom_Step
    { stepX = [0,0.2,0.5]
    , stepY = [0,0.2,0.5]
    , stepSize = 0.005 -- can modify to accept more size and colours
    , stepColour = black
    , stepAlpha = 0.7
  --, stepLinetype ::
    , stepCap = LineCapRound
    , stepJoin = LineJoinRound
    }

transformstep :: Geom_Step -> (Double, Double) -> Geom_Step
transformstep (Geom_Step x y size colour alpha cap join) (w, h) = Geom_Step (map (multiply' w) x) (map (multiply' h) y) (mean*size) colour alpha cap join
                                    where mean = (w + h)
drawstepS :: Geom_Step -> DiaR2
drawstepS (Geom_Step x y size colour alpha cap join) = fromVertices (map p2 (createstep x y)) # lwG size # lcA (colour `withOpacity` alpha) # lineCap cap # lineJoin join 

createstep :: [a] -> [a] -> [(a,a)]
createstep [] _ = []
createstep _ [] = []
createstep (x:xs) (y:ys) = (x,y):(createstep' xs (y:ys))

createstep' :: [a] -> [a] -> [(a,a)]
createstep' [] _ = []
createstep' _ [] = []
createstep' (x:xs) (y:ys) = (x,y):(createstep (x:xs) ys)

drawstepM' :: [Geom_Step] -> DiaR2
drawstepM' pdata = mconcat [drawstepS a| a <- pdata]

stepplot' :: DataFrame -> DataFrame -> Geom_Step
stepplot' d1 d2 = Geom_Step (createdata d1) (createdata d2) 0.003 blue 0.8 LineCapRound LineJoinRound 

stepplot :: DataFrame -> DataFrame -> Double -> Colour Double -> Double -> Geom_Step
stepplot d1 d2 size colour alpha = Geom_Step (createdata d1) (createdata d2) size colour alpha LineCapRound LineJoinRound

stepplotgroup :: DataFrame -> DataFrame -> DataFrame -> Double -> [Geom_Step]
stepplotgroup d1 d2 d3 size = createstepdata (createdataM d1 d3) (createdataM d2 d3) (repeat size) (domaindata d3 colourScale) (repeat 0.7)

createstepdata :: [[Double]] -> [[Double]] -> [Double] -> [Colour Double] -> [Double] -> [Geom_Step]
createstepdata x y size colour alpha = [(Geom_Step (x!!i) (y!!i) (size!!i) (colour!!i) (alpha!!i) LineCapRound LineJoinRound)| i <- [0..((length x) - 1)]] --change

