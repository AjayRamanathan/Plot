{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Plots.Api
    ( ShapeSystem(..)
    , SizeSystem(..)
    , ColourSystem(..)
    , AlphaSystem(..)
    ) where

import Control.Lens (makeLenses, (^.))
import Data.Default
import Data.List

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

class ShapeSystem a where
    shapesystem :: a -> [PointShape]

instance ShapeSystem DataFrame where
    shapesystem a = mapdisContinous a shapeScale  --- currently only 3 domain

instance ShapeSystem PointShape where
    shapesystem a = repeat a

------------ size -----------
class SizeSystem a where
    sizesystem :: a -> [Double]

instance SizeSystem DataFrame where
    sizesystem a = mapContinousDouble a sizeScale 

instance SizeSystem Double where
    sizesystem a = repeat a

------ colour --------
class ColourSystem a where
    coloursystem :: a -> [Colour Double]

instance ColourSystem DataFrame where
    coloursystem a = mapdisContinous a colourScale -- 5 domain -- add continous

instance ColourSystem (Colour Double) where
    coloursystem a = repeat a

---- alpha -- havent used it anywhere
class AlphaSystem a where
    alphasystem :: a -> [Double]

instance AlphaSystem DataFrame where
    alphasystem a = mapContinousDouble a alphaScale -- 5 domain

instance AlphaSystem Double where
    alphasystem a = repeat a
---- end -------
