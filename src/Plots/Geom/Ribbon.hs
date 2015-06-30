{-# LANGUAGE TemplateHaskell #-}

module Plots.Geom.Ribbon
    ( Geom_Ribbon(..)
    , createribbondata
    , ribbonplotgroup
    , ribbonplot
    , ribbonplot'
    , createareadata
    , areaplot
    , areaplot'
    , drawribbonM'
    , drawribbonS 
    , transformribbon 
    ) where

import Control.Lens (makeLenses, (^.))
import Data.Default (Default, def)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)

import Diagrams.Prelude
import Diagrams.TwoD.Path
import Diagrams.Attributes

import Plots.Dataset
import Plots.Types
import Plots.Utils

---------------------------------------------

data Geom_Ribbon = Geom_Ribbon 
    { ribbonX :: [Double]
    , ribbonYmax :: [Double]
    , ribbonYmin :: [Double]
    , ribbonColour :: Colour Double -- can modify to accept more colours
    , ribbonAlpha :: Double
    }

instance Default Geom_Ribbon where
  def = Geom_Ribbon
    { ribbonX = [0,0.2,0.5]
    , ribbonYmax = [0.4,0.6,0.9]
    , ribbonYmin = [0.0, 0.0, 0.0] -- repeat 0 doesnt work
    , ribbonColour = blue
    , ribbonAlpha = 0.7
    }

transformribbon :: Geom_Ribbon -> (Double, Double) -> Geom_Ribbon
transformribbon (Geom_Ribbon x ymin ymax colour alpha) (w, h) = Geom_Ribbon (map (multiply' w) x) (map (multiply' h) ymin) (map (multiply' h) ymax) colour alpha

drawribbonS :: Geom_Ribbon -> DiaR2
drawribbonS (Geom_Ribbon x ymin ymax colour alpha) = path # closeLine # strokeLoop # lw none # fcA (colour `withOpacity` alpha) #translate (r2 (xlast, yminlast))
                                                   where path = fromVertices (append' (maker' x ymax) (maker' x ymin))
                                                         xlast = head x
                                                         yminlast = head ymax

drawribbonM' :: [Geom_Ribbon] -> DiaR2
drawribbonM' pdata = mconcat [drawribbonS a| a <- pdata]

areaplot' :: DataFrame -> DataFrame -> Geom_Ribbon
areaplot' d1 d2 = Geom_Ribbon (createdata d1) (createdata d2) (repeat 0) blue 0.8

areaplot :: DataFrame -> DataFrame -> Colour Double -> Double -> Geom_Ribbon
areaplot d1 d2 colour alpha = Geom_Ribbon (createdata d1) (createdata d2) (repeat 0) colour alpha

areaplotgroup :: DataFrame -> DataFrame -> DataFrame -> [Geom_Ribbon]
areaplotgroup d1 d2 d3 = createareadata (createdataM d1 d3) (createdataM d2 d3) (domaindata d3 colourScale) (repeat 0.7)

createareadata :: [[Double]] -> [[Double]] -> [Colour Double] -> [Double] -> [Geom_Ribbon]
createareadata x y colour alpha = [(Geom_Ribbon (x!!i) (y!!i) (repeat 0) (colour!!i) (alpha!!i))| i <- [0..((length x) - 1)]] --change

ribbonplot' :: DataFrame -> DataFrame -> DataFrame -> Geom_Ribbon
ribbonplot' d1 d2 d3 = Geom_Ribbon (createdata d1) (createdata d2) (createdata d3) blue 0.8

ribbonplot :: DataFrame -> DataFrame -> DataFrame -> Colour Double -> Double -> Geom_Ribbon
ribbonplot d1 d2 d3 colour alpha = Geom_Ribbon (createdata d1) (createdata d2) (createdata d3) colour alpha

ribbonplotgroup :: DataFrame -> DataFrame -> DataFrame -> DataFrame -> [Geom_Ribbon]
ribbonplotgroup x ymin ymax d  = createribbondata (createdataM x d) (createdataM ymin d) (createdataM ymax d) (domaindata d colourScale) (repeat 0.7)

createribbondata :: [[Double]] -> [[Double]] -> [[Double]] -> [Colour Double] -> [Double] -> [Geom_Ribbon]
createribbondata x ymax ymin colour alpha = [(Geom_Ribbon (x!!i) (ymax!!i) (ymin!!i) (colour!!i) (alpha!!i))| i <- [0..((length x) - 1)]]  --change

-- api for area, ribbon and group both
