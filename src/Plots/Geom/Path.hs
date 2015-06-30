{-# LANGUAGE TemplateHaskell #-}

module Plots.Geom.Path 
    ( line
    , LineOpts
    , lineshape 

    , Geom_Path(..)
    , transformpath
    , drawpathS
    , drawpathM
    , drawpathM'
    , pathplot'
    , pathplot
    , pathplotgroup
    , createpathdata
    ) where

import Control.Lens (makeLenses, (^.))
import Data.Default
import Data.Maybe

import Diagrams.Prelude

import Plots.Types
import Plots.Utils (hasNaN)

data LineOpts = LineOpts
    { _lineshape :: Char
    }

makeLenses ''LineOpts

instance Default LineOpts where
    def = LineOpts
        { _lineshape = 'o'
        }

line :: (PlotData m1 a1, PlotData m2 a2) => m1 a1 -> m2 a2 -> LineOpts -> PlotFunction
line xs ys opt mapX mapY | hasNaN xy = error "something: NaN"
                            | otherwise = [l]
  where
    l = lwO 1 . fromVertices . map p2 . mapMaybe (runMap pMap) $ xy
    xy = zip (getValues xs) $ getValues ys
    pMap = compose mapX mapY
 ----------------------------------------------------------
data Geom_Path = Geom_Path
    { pathX :: [Double]
    , pathY :: [Double]
    , pathSize :: Double
    , pathColour :: Colour Double
    , pathAlpha :: Double
 -- , pathLineType :: LineType
    , pathCap :: LineCap
    , pathJoin :: LineJoin
    }

--perhaps in future add colour as property so it can have complex gradient

instance Default Geom_Path where
  def = Geom_Path
    { pathX = [0,0.5,1]
    , pathY = [0,0.5,1]
    , pathSize = 0.005 -- can modify to accept more size and colours
    , pathColour = black
    , pathAlpha = 0.7
 -- , pathLineType = LineSolid
    , pathCap = LineCapRound
    , pathJoin = LineJoinRound
    }

--data LineType = LineSolid   --  solid line
--              | LineDashed [Double] -- dashed line

transformpath :: Geom_Path -> (Double, Double) -> Geom_Path
transformpath (Geom_Path x y size colour alpha cap join) (w, h) = Geom_Path (map (multiply' w) x) (map (multiply' h) y) (mean*size) colour alpha cap join
                                    where mean = (w + h)

drawpathS :: Geom_Path -> DiaR2
drawpathS (Geom_Path x y size colour alpha cap join) = fromVertices (map p2 (zip x y)) # lwG size # lcA (colour `withOpacity` alpha) # lineCap cap # lineJoin join 

drawpathM :: [Geom_Path] -> DiaR2
drawpathM pdata = mconcat [fromVertices (map p2 (zip x y)) # lwG size # lcA (colour `withOpacity` alpha) # lineCap cap # lineJoin join | (Geom_Path x y size colour alpha cap join) <- pdata]

drawpathM' :: [Geom_Path] -> DiaR2
drawpathM' pdata = mconcat [drawpathS a| a <- pdata]

pathplot' :: DataFrame -> DataFrame -> Geom_Path
pathplot' d1 d2 = Geom_Path (createdata d1) (createdata d2) 0.003 blue 0.8 LineCapRound LineJoinRound

pathplot :: DataFrame -> DataFrame -> Double -> Colour Double -> Double -> Geom_Path
pathplot d1 d2 size colour alpha = Geom_Path (createdata d1) (createdata d2) size colour alpha LineCapRound LineJoinRound

pathplotgroup :: DataFrame -> DataFrame -> DataFrame -> Double -> [Geom_Path]
pathplotgroup d1 d2 d3 size = createpathdata (createdataM d1 d3) (createdataM d2 d3) (repeat size) (domaindata d3 colourScale) (repeat 0.7)

createpathdata :: [[Double]] -> [[Double]] -> [Double] -> [Colour Double] -> [Double] -> [Geom_Path]
createpathdata x y size colour alpha = [(Geom_Path (x!!i) (y!!i) (size!!i) (colour!!i) (alpha!!i) LineCapRound LineJoinRound)| i <- [0..((length x) - 1)]] --change
-- api -- api for group



