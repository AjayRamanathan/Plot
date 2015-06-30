{-# LANGUAGE TemplateHaskell #-}

module Plot.Geom.AbLine
    (-- points
    --, PointOpts
    --, shape
    ) where

import Diagrams.Prelude
import Data.Default
import Control.Lens (makeLenses, (^.))
import Data.Maybe

import Diagrams.Plots.Typ

--------------------------------------------

--data Geom_abline = Geom_abline
--    { intercept :: Double
--    , slope :: Double
--    , ablineSize :: Double
--    , ablineColour :: Colour Double
 --   , ablineAlpha :: Double
    --, ablineType :: LineType
 --   }

--vline and hline function


----------------------------------------------
{-
data Geom_Segment = Geom_Segment
    { segmentX :: 
    , segmentY ::
    , segmentXend ::
    , segmentYend ::
    , segmentalpha ::
    , segmentcolour ::
    , segmentlinetype ::
    , segmentsize ::
--    , segmentarrowend ::
    }
-------------------------

data Geom_text = Geom_text
    { textlabel ::
    , textX :: 
    , textY ::
    , textalpha ::
    , textcolour ::
    , textangle  ::
    , family ::
    , fontface ::
    , size ::
    , anchor ::
    }

--- density, function, frequency and smooth --contour
--polar --pie --line --abline --ribbon --path --point
--rectangle --raster --smoothraster --tile 
