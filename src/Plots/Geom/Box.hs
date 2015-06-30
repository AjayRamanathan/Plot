{-# LANGUAGE TemplateHaskell #-}

module Plots.Geom.Box
    ( Geom_Box(..)

    , transformbox
    , drawboxS
    , drawboxM'
    ) where

import Control.Lens (makeLenses, (^.))
import Data.Default
import Data.Maybe
import Data.List

import Diagrams.Prelude

import Plots.Types
import Plots.Utils

data Geom_Box = Geom_Box
    { boxX :: Double
    , boxY :: Double
    , boxWidth :: Double
    , boxHeight :: Double
    , boxSize :: Double
    , boxColour :: Colour Double
    , boxAlpha :: Double
    , boxFillcolour :: Colour Double
    , boxFillalpha :: Double
    }

instance Default Geom_Box where
  def = Geom_Box
    { boxX = 0.5
    , boxY = 0.5
    , boxWidth = 0.1
    , boxHeight = 0.2
    , boxSize = 0.005
    , boxColour = black
    , boxAlpha = 1
    , boxFillcolour = blue
    , boxFillalpha = 0.7
    }
--maybe add fillcolour and fillalpha --linetype

transformbox :: Geom_Box -> (Double, Double) -> Geom_Box
transformbox (Geom_Box x y wdth hght bsize colour alpha fcolour falpha) (w, h) = Geom_Box (w*x) (h*y) (w*wdth) (h*hght) (mean*bsize) colour alpha fcolour falpha
                                    where mean = (w + h)
drawboxS :: Geom_Box -> DiaR2
drawboxS (Geom_Box x y wdth hght bsize colour alpha fcolour falpha) = rect wdth hght # fcA (fcolour `withOpacity` falpha) # lcA (colour `withOpacity` alpha) # lwG bsize #moveTo (p2 (x,y))

drawboxM' :: [Geom_Box] -> DiaR2
drawboxM' pdata = mconcat [drawboxS a| a <- pdata]

---------------------------------------------------
