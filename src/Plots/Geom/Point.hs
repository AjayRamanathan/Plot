{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Plots.Geom.Point
    ( points
    , PointOpts
    , shape
    
    , Geom_Point(..)

    , createpointdata
    , pointplot
    , drawpointM 
    , transformdrawpoint
    , transformpointM
    ) where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Core.Points
import Diagrams.Attributes

import Control.Lens (makeLenses, (^.))
import Data.Default
import Data.List
import Data.Maybe

import Plots.Types
import Plots.Utils

data PointOpts = PointOpts
    { _shape :: Char
    }

makeLenses ''PointOpts

instance Default PointOpts where
    def = PointOpts
        { _shape = 'o'
        }

points :: (PlotData m1 a1, PlotData m2 a2) => m1 a1 -> m2 a2 -> PointOpts -> PlotFunction
points xs ys opt mapX mapY = map (uncurry moveTo) ps
  where
    ps = flip zip (repeat s).map p2.mapMaybe (runMap pMap) $ xy
    xy = zip (getValues xs) $ getValues ys
    s = lwO 1 $ stroke.getShape $ opt^.shape
    pMap = compose mapX mapY

getShape :: Char -> Path V2 Double
{-# INLINE getShape #-}
getShape s | s == 'o' = circle 0.07
           | s == '^' = eqTriangle 0.1
           | s == '#' = square 0.1
           | s == '+' = plus 0.07
           | s == '*' = star (StarSkip 2) (pentagon 0.1)
           | s == 'x' = cross 0.07
           | otherwise = circle 0.07

cross :: Double -> Path V2 Double
{-# INLINE cross #-}
cross x = fromVertices [ x^&(-x) , (-x)^&x ]
          <> fromVertices [ x^&x , (-x)^&(-x) ]

plus :: Double -> Path V2 Double
{-# INLINE plus #-}
plus x = cross x # rotate (45 @@ deg)
-------------------------- Geom Point -------------------------------------------------

data Geom_Point = Geom_Point 
    { pointX :: Double
    , pointY :: Double
    , pointSize :: Double
    , pointColour :: Colour Double
    , pointAlpha :: Double
    , pointBorderSize :: Double
    , pointBorderColour :: Colour Double
    , pointShape :: PointShape
    }

data PointShape = PointShapeCircle   --  A circle.
                | PointShapeTriangle -- A Triangle
                | PointShapeSquare -- Square

-- add more shapes, with parameters

shapeScale :: [PointShape]
shapeScale = [PointShapeCircle, PointShapeTriangle, PointShapeSquare]

shapedata = mapdisContinous species shapeScale

instance Default Geom_Point where
  def = Geom_Point
    { pointX = 0
    , pointY = 0
    , pointSize = 0.1
    , pointColour = black
    , pointAlpha = 0.7
    , pointBorderSize = 0.003
    , pointBorderColour = black
    , pointShape = PointShapeCircle -- work on this either add more shapes and adjust or change
    }

transformpointM :: [Geom_Point] -> (Double, Double) -> [Geom_Point]
transformpointM pdata (w, h) = [(Geom_Point (w*x) (h*y) (mean*size) colour alpha (mean*bsize) bcolour shape) | Geom_Point x y size colour alpha bsize bcolour shape <- pdata]
                                    where mean = (w + h)/2

transformdrawpoint :: [Geom_Point] -> (Double, Double) -> DiaR2
transformdrawpoint pointdata (w, h) = drawpointM (transformpointM pointdata (w, h)) 

drawpointM :: [Geom_Point] -> DiaR2
drawpointM pdata = position (zip (map makepoint [(x, y)| Geom_Point x y _ _ _ _ _ _ <- pdata]) [(getShape shape) size #fcA (colour `withOpacity` alpha) #lc bcolour #lwG bsize | Geom_Point _ _ size colour alpha bsize bcolour shape <- pdata])
                 where getShape PointShapeCircle  = circle
                       getShape PointShapeTriangle  = triangle
                       getShape PointShapeSquare  = square   --- change this maybe add seperate code for size problems

--- api --------

pointplot :: (SizeSystem c, ColourSystem d, ShapeSystem e, Fractional c) => DataFrame -> DataFrame -> c -> d -> e -> [Geom_Point]
pointplot a b c d e = (createpointdata (createdata a) (createdata b) (sizesystem c) (coloursystem d) (shapesystem e))

createpointdata :: [Double] -> [Double] -> [Double] -> [Colour Double] -> [PointShape] -> [Geom_Point]
createpointdata a b c d e = [Geom_Point (a!!i) (b!!i) (c!!i) (d!!i) 0.3 0.003 (d!!i) (e!!i) | i <- [0..((length a) - 1)]] --- change this 

{- explore futher shapes Trail V2 Double, also compensate for size, make api for scatterplot

filledCiralphacolores :: Double -> AlphaColour Double -> PointOpts
filledCiralphacolores radius alphacolor = 
  PointOpts alphacolor transparent 0 radius PointShapeCiralphacolore


hollowCiralphacolores :: Double -- radius
              -> Double -- size.
              -> AlphaColour Double -- color
              -> PointOpts
hollowCiralphacolores radius size alphacolor = 
  PointOpts transparent alphacolor size radius PointShapeCiralphacolore

hollowPolygon :: Double -- radius
              -> Double -- size
              -> Int    -- n
              -> Bool   -- upside
              -> AlphaColour Double -- color
              -> PointOpts
hollowPolygon radius size sides right alphacolor = 
  PointOpts transparent alphacolor size radius (PointShapePolygon sides right)

filledPolygon :: Double -> Int -> Bool -> AlphaColour Double -> PointOpts
filledPolygon radius sides right alphacolor = 
  PointOpts alphacolor transparent 0 radius (PointShapePolygon sides right)

-- plus
plusss :: Double -- radius
       -> Double -- size
       -> AlphaColour Double -- size
       -> PointOpts
plusss radius size alphacolor = 
  PointOpts transparent alphacolor size radius PointShapePlus

-- cross
xs :: Double -- radius
   -> Double -- size
   -> AlphaColour Double -- colur
   -> PointOpts
xs radius size alphacolor =
  PointOpts transparent alphacolor size radius PointShapeCross

-- stars
stars :: Double -- radius
      -> Double -- size
      -> AlphaColour Double 
      -> PointOpts
stars radius size alphacolor =
  PointOpts transparent alphacolor size radius PointShapeStar


---each has definitions, tranform & draw.M.S, and api
