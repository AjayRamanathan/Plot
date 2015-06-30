{-# LANGUAGE FlexibleContexts #-}

module Plots.Utils
    ( doubleparts
    , multiply'
    , relate
    , append'
    , maker'

    , shapeScale
    , alphaScale
    , sizeScale
    , colourScale
    , shapedata
    , colourdata

    , DiaR2
    
    , mapdisContinous
    , mapContinousDouble

    , createdata
    , createdataM 
    , domaindata
    
    , makepoint
    , createDoublelist
    , ratiowh

    , projection

    , autoSteps
    , linearMap
    , linearMapBound
    , hasNaN

    , text'
    ) where

import Data.Ord (comparing)
import Data.Function
import Data.List

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Plots.Dataset
import Plots.Types

coordCartesian :: DataFrame -> DataFrame -> (Double, Double) -> DiaR2
coordCartesian (DataDouble a1 b1 c1) (DataDouble a2 b2 c2) (w,h) = position (zip (map makepoint [(-0.85,0),(0,-0.85),(0,0)]) [text a1 # fontSizeL 0.1 # rotateBy (1/4), text a2 # fontSizeL 0.1, (verticksD (createDoublelist c2 doubleparts) h <> horizonsD (createDoublelist c1 doubleparts) w)]) # translate (r2 (0.5, 0.5))  <> (rect w h # translate (r2 (w/2, h/2)) #fc white)
coordCartesian (DataString a1 b1 c1) (DataDouble a2 b2 c2) (w,h) = position (zip (map makepoint [(-0.85,0),(0,-0.85),(0,0)]) [text a1 # fontSizeL 0.1 # rotateBy (1/4), text a2 # fontSizeL 0.1, (verticksD (createDoublelist c2 doubleparts) h <> horizonsS c1 w)]) # translate (r2 (0.5, 0.5))  <> (rect w h # translate (r2 (w/2, h/2)) #fc white)
coordCartesian (DataDouble a1 b1 c1) (DataString a2 b2 c2) (w,h) = position (zip (map makepoint [(-0.85,0),(0,-0.85),(0,0)]) [text a1 # fontSizeL 0.1 # rotateBy (1/4), text a2 # fontSizeL 0.1, (verticksS c2 h <> horizonsD(createDoublelist c1 doubleparts) w)]) # translate (r2 (0.5, 0.5))  <> (rect w h # translate (r2 (w/2, h/2)) #fc white)
coordCartesian (DataString a1 b1 c1) (DataString a2 b2 c2) (w,h) = position (zip (map makepoint [(-0.85,0),(0,-0.85),(0,0)]) [text a1 # fontSizeL 0.1 # rotateBy (1/4), text a2 # fontSizeL 0.1, (verticksS c2 h <> horizonsS c1 w)]) # translate (r2 (0.5, 0.5)) <> (rect w h # translate (r2 (w/2, h/2)) #fc white)

verticksD :: [String] -> Double -> DiaR2
verticksD ys h = position (zip ([makepoint (-0.52,-0.5)]) [(mconcat [ text t # fontSizeL 0.025 # moveTo ((-0.14)^&(y*h)) | (y,t) <- ylist]) <> (mconcat [fromVertices [0.0^&(y*h), 0.02^&(y*h)] | (y,t) <- ylist])] )
               where ylist = zip [(fromIntegral d)/(fromIntegral ((length ys)-1)) | d <- [0..((length ys) - 1)]] ys

verticksS :: [String] -> Double -> DiaR2
verticksS ys h = position (zip ([makepoint (-0.52,-0.5)]) [(mconcat [ text t # fontSizeL 0.025 # moveTo ((-0.14)^&(y*h)) | (y,t) <- ylist]) <> (mconcat [fromVertices [0.0^&(y*h), 0.02^&(y*h)] | (y,t) <- ylist])] ) 
               where ylist = zip [(fromIntegral d)/(fromIntegral ((length ys)+1)) | d <- [1..(length ys)]] ys

horizonsD :: [String] -> Double -> DiaR2
horizonsD xs w = position (zip ([makepoint (-0.5,-0.52)]) [(mconcat [ text t # fontSizeL 0.025  # rotateBy (1/4) # moveTo ((x*w)^&(-0.14)) | (x,t) <- xlist]) <> (mconcat [fromVertices [(x*w)^&0.0, (x*w)^&0.02] | (x,t) <- xlist])] )
               where xlist = zip [(fromIntegral d)/(fromIntegral ((length xs)-1)) | d <- [0..((length xs) - 1)]] xs

horizonsS :: [String] -> Double -> DiaR2
horizonsS xs w = position (zip ([makepoint (-0.5,-0.52)]) [(mconcat [ text t # fontSizeL 0.025  # rotateBy (1/4) # moveTo ((x*w)^&(-0.14)) | (x,t) <- xlist]) <> (mconcat [fromVertices [(x*w)^&0.0, (x*w)^&0.02] | (x,t) <- xlist])] )
               where xlist = zip [(fromIntegral d)/(fromIntegral ((length xs)+1)) | d <- [1..(length xs)]] xs

---- crop at w and h

type DiaR2 = Diagram B R2

doubleparts = 10.0

multiply' x y = x*y

alphaScale = (0.3, 0.7)
sizeScale = (0.01, 0.02)
colourScale = [red, blue, green, black]
colourdata = mapdisContinous species colourScale

append' :: [a] -> [a] -> [a]
append' a b = a ++ (reverse b)

maker' x y = map p2 (zip x y)

relate :: Eq a => a -> [(a, b)] -> b
relate c = snd.head.dropWhile ((/=c).fst)

mapdisContinous :: DataFrame -> [a] -> [a]
mapdisContinous (DataString _ xs ds) map = [ relate x (zip ds map)| x<-xs]
mapdisContinous (DataDouble _ xs ds) map = error "something"  -- perhaps will work with statbin
 
mapContinousDouble :: DataFrame -> (Double, Double) -> [Double]
mapContinousDouble (DataString _ xs ds) (a,b) = error "something"  -- add something here
mapContinousDouble (DataDouble _ xs (d1, d2)) (a,b) = [a + ((x*(b-a))/(d2-d1)) | x<-xs]

--seperatemapcontinous for colour ? --maybe others too

createdata :: DataFrame -> [Double]
createdata (DataDouble _ xs (a,b)) = [(x-a)/(b-a)| x<-xs]
createdata (DataString _ xs ds) = [(relate x (zip ds [1.0..]))/fromIntegral  ((length ds)+ 1)| x<-xs]

createdataM :: DataFrame -> DataFrame -> [[Double]]
createdataM _ (DataDouble a2 b2 c2) = error "something"
createdataM (DataDouble z xs (a,b)) (DataString _ xs2 ds) = foobarm (createdata (DataDouble z xs (a,b))) xs2 ds
createdataM (DataString z xs ds) (DataString _ xs2 ds2) = foobarm (createdata (DataString z xs ds)) xs2 ds2
 
foobar' xs1 xs2 string = [fst x | x <- (zip xs1 xs2), snd x == string ]
foobarm xs1 xs2 ds = [foobar' xs1 xs2 d | d <- ds]

domaindata :: DataFrame ->  [a] -> [a]
domaindata (DataString _ xs ds) map = [ relate d (zip ds map)| d<-ds]
domaindata (DataDouble _ xs ds) map = error "something"  -- perhaps will work with statbin

makepoint (x,y) = p2 (x,y)

createDoublelist :: (Double,Double) -> Double -> [String]
createDoublelist (a,b) n = [show (a+(i*(b-a)/n)) | i <- [0..n] ]

ratiowh :: (Double, Double)
ratiowh = (2, 1)

-- | project a 3d point to 2d
projection :: (Double, Double, Double)  -- ^ position of camera
           -> (Double, Double, Double)  -- ^ orientation of camera
           -> (Double, Double, Double)  -- ^ viewer's position
           -> (Double, Double, Double)  -- ^ 3d point to be projected
           -> (Double, Double)
projection (cx',cy',cz') (θx,θy,θz) (ex,ey,ez) (ax,ay,az) = (bx, by)
  where
    bx = ez / dz * dx - ex
    by = ez / dz * dy - ey
    dx = cy * (sz * y + cz * x) - sy * z
    dy = sx * (cy * z + sy * (sz * y + cz * x)) + cx * (cz * y - sz * x)
    dz = cx * (cy * z + sy * (sz * y + cz * x)) - sx * (cz * y - sz * x)
    x = ax - cx'
    y = ay - cy'
    z = az - cz'
    sx = sin θx
    sy = sin θy
    sz = sin θz
    cx = cos θx
    cy = cos θy
    cz = cos θz

chooseStep :: RealFloat a => a -> (a,a) -> Rational
chooseStep nsteps (x1,x2) = minimumBy (comparing proximity) stepVals
  where
    delta = x2 - x1
    mult  = 10 ^^ ((floor $ logBase 10 $ delta / nsteps)::Integer)
    stepVals = map (mult*) [0.1,0.2,0.25,0.5,1.0,2.0,2.5,5.0,10,20,25,50]
    proximity x = abs $ delta / realToFrac x - nsteps
{-# INLINE chooseStep #-}

-- | Given a target number of values, and a list of input points,
--   find evenly spaced values from the set {1*X, 2*X, 2.5*X, 5*X} (where
--   X is some power of ten) that evenly cover the input points.
autoSteps :: Int -> (Double, Double) -> (Rational, Rational, Rational)
autoSteps nSteps (minV, maxV) = (min', max', step)
  where
    r@(minV', maxV')  | minV == maxV = (minV-0.5,minV+0.5)
                      | otherwise    = (minV, maxV)
    step = chooseStep (fromIntegral nSteps) r
    min' = fromIntegral (floor   $ realToFrac minV' / step :: Integer) * step
    max' = fromIntegral (ceiling $ realToFrac maxV' / step :: Integer) * step

linearMap :: (Double, Double) -> (Double, Double) -> PointMap Double
linearMap (l, u) (l', u') = PointMap mapFn (l, u)
  where
    mapFn x | x < l || x > u = Nothing
            | otherwise = Just $ (x - l) / (u - l) * (u' - l') + l'
{-# INLINE linearMap #-}

linearMapBound :: (Double, Double) -> (Double, Double) -> PointMap Double
linearMapBound (l, u) (l', u') = PointMap mapFn (l, u)
  where
    mapFn x | x < l = Just l' 
            | x > u = Just u'
            | otherwise = Just $ (x - l) / (u - l) * (u' - l') + l'
{-# INLINE linearMapBound #-}

hasNaN :: [(Double, Double)] -> Bool
hasNaN = any (uncurry ((||) `on` isNaN))

--text' :: Double -> String -> DiaR2
--text' size str = stroke (textSVG' (TextOpts str lin2 INSIDE_WH HADV False size size)) # fc black # lwL 0

--text' :: Double -> String -> DiaR2
--text' size str = textVisualBounded (fontSize (Local size) mempty) str # fontSize (Local size)

text' :: Double -> String -> DiaR2
text' size str = text str # fontSizeL size
