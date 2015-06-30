{-# LANGUAGE TemplateHaskell #-}

module Plots.Geom.Linerange
    ( Geom_LinerangeH(..)
    , transformlinerangeh
    , drawlinerangehS
    , drawlinerangehM'  

    , Geom_LinerangeV(..)
    , transformlinerangev
    , drawlinerangevS  
    , drawlinerangevM'

    , crossbarhplot
    , crossbarhplotwithC
    , crossbarhplot'
    , errorbarvplot
    , errorbarvplot'
    , errorbarvplotM'
    , errorbarhplot
    , errorbarhplot'
    , boxplotplot'
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
import Plots.Geom.Box

data Geom_LinerangeH = Geom_LinerangeH
    { linerangehX :: Double
    , linerangehY :: Double
    , linerangehWidth :: Double
    , linerangehSize :: Double
    , linerangehColour :: Colour Double
    , linerangehAlpha :: Double
 -- , --cap ?
 -- , --linetype
    } 

instance Default Geom_LinerangeH where
  def = Geom_LinerangeH
    { linerangehX = 0.5
    , linerangehY = 0.5
    , linerangehWidth = 0.2
    , linerangehSize = 0.003
    , linerangehColour = black
    , linerangehAlpha = 0.7
 -- , -- cap ?
 -- , --linetype
    } 

transformlinerangeh :: Geom_LinerangeH -> (Double, Double) -> Geom_LinerangeH
transformlinerangeh (Geom_LinerangeH x y wdth size colour alpha) (w, h) = Geom_LinerangeH (w*x) (h*y) (w*wdth) (mean*size) colour alpha
                                                                        where mean = (w + h)

drawlinerangehS :: Geom_LinerangeH -> DiaR2
drawlinerangehS (Geom_LinerangeH x y wdth size colour alpha) = fromVertices (map p2 [(x-a,y),(x+a,y)] ) # lwG size # lcA (colour `withOpacity` alpha)
                                                   where a = (wdth/2)

drawlinerangehM' :: [Geom_LinerangeH] -> DiaR2
drawlinerangehM' pdata = mconcat [drawlinerangehS a| a <- pdata]

------------------------------------------------------------------------------------

data Geom_LinerangeV = Geom_LinerangeV
    { linerangevX :: Double
    , linerangevY :: Double
    , linerangevHeight :: Double
    , linerangevSize :: Double
    , linerangevColour :: Colour Double
    , linerangevAlpha :: Double
 -- , --cap ?
 -- , --linetype
    } 

instance Default Geom_LinerangeV where
  def = Geom_LinerangeV
    { linerangevX = 0.5
    , linerangevY = 0.5
    , linerangevHeight = 0.2
    , linerangevSize = 0.003
    , linerangevColour = black
    , linerangevAlpha = 0.7
 -- , -- cap ?
 -- , --linetype
    } 

transformlinerangev :: Geom_LinerangeV -> (Double, Double) -> Geom_LinerangeV
transformlinerangev (Geom_LinerangeV x y hght size colour alpha) (w, h) = Geom_LinerangeV (w*x) (h*y) (h*hght) (mean*size) colour alpha
                                                                        where mean = (w + h)

drawlinerangevS :: Geom_LinerangeV -> DiaR2
drawlinerangevS (Geom_LinerangeV x y hght size colour alpha) = fromVertices (map p2 [(x,y-a),(x,y+a)] ) # lwG size # lcA (colour `withOpacity` alpha)
                                                   where a = (hght/2)

drawlinerangevM' :: [Geom_LinerangeV] -> DiaR2
drawlinerangevM' pdata = mconcat [drawlinerangevS a| a <- pdata]

crossbarhplot :: Double -> Double -> Double -> Double -> Colour Double -> Colour Double -> Double -> (Double, Double) -> DiaR2
crossbarhplot x y wdth hght colour fcolour falpha ratio = drawlinerangehS (transformlinerangeh (Geom_LinerangeH x y wdth 0.002 colour 1) ratio) <> (drawboxS (transformbox (Geom_Box x y wdth hght 0.002 colour 1 fcolour falpha) ratio)) 

crossbarhplotwithC :: Double -> Double -> Double -> Double -> Double -> Colour Double -> Colour Double -> Double -> (Double, Double) -> DiaR2
crossbarhplotwithC x y wdth hght adj colour fcolour falpha ratio = drawlinerangehS (transformlinerangeh (Geom_LinerangeH x (y+adj) wdth 0.002 colour 1) ratio) <> (drawboxS (transformbox (Geom_Box x y wdth hght 0.002 colour 1 fcolour falpha) ratio))

crossbarhplot' :: Double -> Double -> Double -> Double -> (Double, Double) -> DiaR2
crossbarhplot' x y wdth hght ratio = drawlinerangehS (transformlinerangeh (Geom_LinerangeH x y wdth 0.002 black 1) ratio) <> (drawboxS (transformbox (Geom_Box x y wdth hght 0.002 black 1 green 0.3) ratio))

crossbarvplot :: Double -> Double -> Double -> Double -> Colour Double -> Colour Double -> Double -> (Double, Double) -> DiaR2
crossbarvplot x y wdth hght colour fcolour falpha ratio = drawlinerangevS (transformlinerangev (Geom_LinerangeV x y hght 0.002 colour 1) ratio) <> (drawboxS (transformbox (Geom_Box x y wdth hght 0.002 colour 1 fcolour falpha) ratio))

crossbarvplot' :: Double -> Double -> Double -> Double -> (Double, Double) -> DiaR2
crossbarvplot' x y wdth hght ratio = drawlinerangevS (transformlinerangev (Geom_LinerangeV x y hght 0.002 black 1) ratio) <> (drawboxS (transformbox (Geom_Box x y wdth hght 0.002 black 1 green 0.3) ratio))

errorbarvplot :: Double -> Double -> Double -> Double -> Colour Double  -> Double -> (Double, Double) -> DiaR2
errorbarvplot x y wdth hght colour alpha ratio = drawlinerangehS (transformlinerangeh (Geom_LinerangeH x (y+w) wdth 0.002 colour alpha) ratiowh) <> drawlinerangehS (transformlinerangeh (Geom_LinerangeH x (y-w) wdth 0.002 colour alpha) ratiowh) <> drawlinerangevS (transformlinerangev (Geom_LinerangeV x y wdth 0.002 colour alpha) ratiowh)
                                               where w = wdth/2
                                                     h = hght/2
errorbarvplot' :: Double -> Double -> Double -> Double -> (Double, Double) -> DiaR2
errorbarvplot' x y wdth hght ratio = errorbarvplot x y wdth hght black 0.7 ratio

errorbarvplotM' :: DataFrame -> DataFrame -> (Double, Double) -> DiaR2
errorbarvplotM' d1 d2 ratio = (mconcat [(errorbarhplot x y 0.05 0.05 black 1 ratio)| (x,y)<-(zip (createdata d1) (createdata d2))])

errorbarhplot :: Double -> Double -> Double -> Double -> Colour Double  -> Double -> (Double, Double) -> DiaR2
errorbarhplot x y wdth hght colour alpha ratio = drawlinerangevS (transformlinerangev (Geom_LinerangeV (x+h) y hght 0.002 colour alpha) ratiowh) <> drawlinerangevS (transformlinerangev (Geom_LinerangeV (x-h) y hght 0.002 colour alpha) ratiowh) <> drawlinerangehS (transformlinerangeh (Geom_LinerangeH x y hght 0.002 colour alpha) ratiowh)
                                               where w = wdth/2
                                                     h = hght/2

errorbarhplot' :: Double -> Double -> Double -> Double -> (Double, Double) -> DiaR2
errorbarhplot' x y wdth hght ratio = errorbarhplot x y wdth hght black 0.7 ratio

boxplotplot' :: Double -> Double -> Double -> Double -> Double -> Double -> Colour Double -> Colour Double -> Double -> (Double, Double) -> DiaR2
boxplotplot' x y wdth hght adj lgth colour fcolour falpha ratio = (crossbarhplotwithC x y wdth hght adj colour fcolour falpha ratio) <> drawlinerangevS (transformlinerangev (Geom_LinerangeV x (y+hght) lgth 0.002 colour 1) ratiowh) <> drawlinerangevS (transformlinerangev (Geom_LinerangeV x (y-hght) lgth 0.002 colour 1) ratiowh)

{-

--api for crossbar and boxplot --error bars

---error bar
-something like this
data Geom_ErrorbarV = Geom_ErrorbarV
    { errorbarX :: 
    , errorbarXmax ::
    , errorbarXmin ::
    , errorbarY ::
    , errorbaralpha ::
    , errorbarcolour ::
    , errorbarheight ::
    , errorbarsize ::
    }

data Geom_ErrorbarH = Geom_ErrorbarH
    { errorbarX :: 
    , errorbarYmax ::
    , errorbarYmin ::
    , errorbarY ::
    , errorbaralpha ::
    , errorbarcolour ::
    , errorbarheight ::
    , errorbarsize ::
    }
-something like this
-}
