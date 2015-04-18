{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Text
import Diagrams.Core.Points

type DiaR2 = Diagram B R2

data DataFrame = DataDouble String [Double] (Double,Double) | DataString String [String] [String]

iris = [(5.1,3.5,1.4,0.2,"setosa","A"),
        (4.9,3.0,1.4,0.2,"setosa","A"),
        (4.7,3.2,1.3,0.2,"setosa","A"),
        (4.6,3.1,1.5,0.2,"setosa","A"),
        (5.0,3.6,1.4,0.2,"setosa","A"),
        (5.4,3.9,1.7,0.4,"setosa","B"),
        (4.6,3.4,1.4,0.3,"setosa","B"),
        (5.0,3.4,1.5,0.2,"setosa","B"),
        (4.4,2.9,1.4,0.2,"setosa","B"),
        (4.9,3.1,1.5,0.1,"setosa","B"),
        (5.4,3.7,1.5,0.2,"setosa","B"),
        (4.8,3.4,1.6,0.2,"setosa","B"),
        (4.8,3.0,1.4,0.1,"setosa","B"),
        (4.3,3.0,1.1,0.1,"setosa","B"),
        (5.8,4.0,1.2,0.2,"setosa","B"),
        (5.7,4.4,1.5,0.4,"setosa","B"),
        (5.4,3.9,1.3,0.4,"setosa","D"),
        (5.1,3.5,1.4,0.3,"setosa","D"),
        (5.7,3.8,1.7,0.3,"setosa","D"),
        (5.1,3.8,1.5,0.3,"setosa","D"),
        (5.4,3.4,1.7,0.2,"setosa","D"),
        (5.1,3.7,1.5,0.4,"setosa","D"),
        (4.6,3.6,1.0,0.2,"setosa","B"),
        (5.1,3.3,1.7,0.5,"setosa","B"),
        (4.8,3.4,1.9,0.2,"setosa","D"),
        (5.0,3.0,1.6,0.2,"setosa","F"),
        (5.0,3.4,1.6,0.4,"setosa","C"),
        (5.2,3.5,1.5,0.2,"setosa","C"),
        (5.2,3.4,1.4,0.2,"setosa","C"),
        (4.7,3.2,1.6,0.2,"setosa","C"),
        (4.8,3.1,1.6,0.2,"setosa","C"),
        (5.4,3.4,1.5,0.4,"setosa","F"),
        (5.2,4.1,1.5,0.1,"setosa","F"),
        (5.5,4.2,1.4,0.2,"setosa","D"),
        (4.9,3.1,1.5,0.1,"setosa","D"),
        (5.0,3.2,1.2,0.2,"setosa","D"),
        (5.5,3.5,1.3,0.2,"setosa","E"),
        (4.9,3.1,1.5,0.1,"setosa","E"),
        (4.4,3.0,1.3,0.2,"setosa","E"),
        (5.1,3.4,1.5,0.2,"setosa","C"),
        (5.0,3.5,1.3,0.3,"setosa","C"),
        (4.5,2.3,1.3,0.3,"setosa","C"),
        (4.4,3.2,1.3,0.2,"setosa","C"),
        (5.0,3.5,1.6,0.6,"setosa","E"),
        (5.1,3.8,1.9,0.4,"setosa","F"),
        (4.8,3.0,1.4,0.3,"setosa","F"),
        (5.1,3.8,1.6,0.2,"setosa","F"),
        (4.6,3.2,1.4,0.2,"setosa","C"),
        (5.3,3.7,1.5,0.2,"setosa","C"),
        (5.0,3.3,1.4,0.2,"setosa","C"),
        (7.0,3.2,4.7,1.4,"versicolor","B"),
        (6.4,3.2,4.5,1.5,"versicolor","B"),
        (6.9,3.1,4.9,1.5,"versicolor","B"),
        (5.5,2.3,4.0,1.3,"versicolor","B"),
        (6.5,2.8,4.6,1.5,"versicolor","B"),
        (5.7,2.8,4.5,1.3,"versicolor","E"),
        (6.3,3.3,4.7,1.6,"versicolor","E"),
        (4.9,2.4,3.3,1.0,"versicolor","E"),
        (6.6,2.9,4.6,1.3,"versicolor","E"),
        (5.2,2.7,3.9,1.4,"versicolor","F"),
        (5.0,2.0,3.5,1.0,"versicolor","F"),
        (5.9,3.0,4.2,1.5,"versicolor","F"),
        (6.0,2.2,4.0,1.0,"versicolor","F"),
        (6.1,2.9,4.7,1.4,"versicolor","F"),
        (5.6,2.9,3.6,1.3,"versicolor","F"),
        (6.7,3.1,4.4,1.4,"versicolor","A"),
        (5.6,3.0,4.5,1.5,"versicolor","A"),
        (5.8,2.7,4.1,1.0,"versicolor","A"),
        (6.2,2.2,4.5,1.5,"versicolor","A"),
        (5.6,2.5,3.9,1.1,"versicolor","E"),
        (5.9,3.2,4.8,1.8,"versicolor","E"),
        (6.1,2.8,4.0,1.3,"versicolor","E"),
        (6.3,2.5,4.9,1.5,"versicolor","E"),
        (6.1,2.8,4.7,1.2,"versicolor","E"),
        (6.4,2.9,4.3,1.3,"versicolor","D"),
        (6.6,3.0,4.4,1.4,"versicolor","D"),
        (6.8,2.8,4.8,1.4,"versicolor","A"),
        (6.7,3.0,5.0,1.7,"versicolor","A"),
        (6.0,2.9,4.5,1.5,"versicolor","A"),
        (5.7,2.6,3.5,1.0,"versicolor","A"),
        (5.5,2.4,3.8,1.1,"versicolor","D"),
        (5.5,2.4,3.7,1.0,"versicolor","D"),
        (5.8,2.7,3.9,1.2,"versicolor","D"),
        (6.0,2.7,5.1,1.6,"versicolor","D"),
        (5.4,3.0,4.5,1.5,"versicolor","E"),
        (6.0,3.4,4.5,1.6,"versicolor","E"),
        (6.7,3.1,4.7,1.5,"versicolor","E"),
        (6.3,2.3,4.4,1.3,"versicolor","E"),
        (5.6,3.0,4.1,1.3,"versicolor","F"),
        (5.5,2.5,4.0,1.3,"versicolor","A"),
        (5.5,2.6,4.4,1.2,"versicolor","A"),
        (6.1,3.0,4.6,1.4,"versicolor","A"),
        (5.8,2.6,4.0,1.2,"versicolor","C"),
        (5.0,2.3,3.3,1.0,"versicolor","C"),
        (5.6,2.7,4.2,1.3,"versicolor","C"),
        (5.7,3.0,4.2,1.2,"versicolor","C"),
        (5.7,2.9,4.2,1.3,"versicolor","F"),
        (6.2,2.9,4.3,1.3,"versicolor","E"),
        (5.1,2.5,3.0,1.1,"versicolor","D"),
        (5.7,2.8,4.1,1.3,"versicolor","C"),
        (6.3,3.3,6.0,2.5,"virginica","C"),
        (5.8,2.7,5.1,1.9,"virginica","C"),
        (7.1,3.0,5.9,2.1,"virginica","D"),
        (6.3,2.9,5.6,1.8,"virginica","C"),
        (6.5,3.0,5.8,2.2,"virginica","C"),
        (7.6,3.0,6.6,2.1,"virginica","F"),
        (4.9,2.5,4.5,1.7,"virginica","F"),
        (7.3,2.9,6.3,1.8,"virginica","C"),
        (6.7,2.5,5.8,1.8,"virginica","B"),
        (7.2,3.6,6.1,2.5,"virginica","B"),
        (6.5,3.2,5.1,2.0,"virginica","B"),
        (6.4,2.7,5.3,1.9,"virginica","E"),
        (6.8,3.0,5.5,2.1,"virginica","E"),
        (5.7,2.5,5.0,2.0,"virginica","B"),
        (5.8,2.8,5.1,2.4,"virginica","B"),
        (6.4,3.2,5.3,2.3,"virginica","B"),
        (6.5,3.0,5.5,1.8,"virginica","B"),
        (7.7,3.8,6.7,2.2,"virginica","F"),
        (7.7,2.6,6.9,2.3,"virginica","F"),
        (6.0,2.2,5.0,1.5,"virginica","F"),
        (6.9,3.2,5.7,2.3,"virginica","F"),
        (5.6,2.8,4.9,2.0,"virginica","F"),
        (7.7,2.8,6.7,2.0,"virginica","F"),
        (6.3,2.7,4.9,1.8,"virginica","D"),
        (6.7,3.3,5.7,2.1,"virginica","D"),
        (7.2,3.2,6.0,1.8,"virginica","D"),
        (6.2,2.8,4.8,1.8,"virginica","D"),
        (6.1,3.0,4.9,1.8,"virginica","D"),
        (6.4,2.8,5.6,2.1,"virginica","D"),
        (7.2,3.0,5.8,1.6,"virginica","D"),
        (7.4,2.8,6.1,1.9,"virginica","B"),
        (7.9,3.8,6.4,2.0,"virginica","B"),
        (6.4,2.8,5.6,2.2,"virginica","F"),
        (6.3,2.8,5.1,1.5,"virginica","F"),
        (6.1,2.6,5.6,1.4,"virginica","F"),
        (7.7,3.0,6.1,2.3,"virginica","B"),
        (6.3,3.4,5.6,2.4,"virginica","E"),
        (6.4,3.1,5.5,1.8,"virginica","E"),
        (6.0,3.0,4.8,1.8,"virginica","E"),
        (6.9,3.1,5.4,2.1,"virginica","E"),
        (6.7,3.1,5.6,2.4,"virginica","B"),
        (6.9,3.1,5.1,2.3,"virginica","B"),
        (5.8,2.7,5.1,1.9,"virginica","A"),
        (6.8,3.2,5.9,2.3,"virginica","A"),
        (6.7,3.3,5.7,2.5,"virginica","A"),
        (6.7,3.0,5.2,2.3,"virginica","E"),
        (6.3,2.5,5.0,1.9,"virginica","E"),
        (6.5,3.0,5.2,2.0,"virginica","E"),
        (6.2,3.4,5.4,2.3,"virginica","A"),
        (5.9,3.0,5.1,1.8,"virginica","A") ]

sepalLength = DataDouble "sepal length" [a | (a,_,_,_,_,_)<-iris] (4.0,8.0)
sepalWidth  = DataDouble "sepal width"  [a | (_,a,_,_,_,_)<-iris] (2.0,5.0)
petalLength = DataDouble "petal length" [a | (_,_,a,_,_,_)<-iris] (1.0,7.0)
petalWidth  = DataDouble "petal width"  [a | (_,_,_,a,_,_)<-iris] (0.0,3.0)
species     = DataString "species"      [a | (_,_,_,_,a,_)<-iris] ["setosa","versicolor","virginica"]
typeall     = DataString "type all"     [a | (_,_,_,_,_,a)<-iris] ["A","B","C","D","E","F"]

doubleparts = 10.0
makepoint (x,y) = p2 (x,y)

createDoublelist :: (Double,Double) -> Double -> [String]
createDoublelist (a,b) n = [show (a+(i*(b-a)/n)) | i <- [0..n] ]

coordCartesian :: DataFrame -> DataFrame -> DiaR2
coordCartesian (DataDouble a1 b1 c1) (DataDouble a2 b2 c2) = position (zip (map makepoint [(-0.85,0),(0,-0.85),(0,0)]) [text a1 # fontSizeL 0.1 # rotateBy (1/4), text a2 # fontSizeL 0.1, (verticksD (createDoublelist c1 doubleparts) <> horizonsD (createDoublelist c2 doubleparts))])
coordCartesian (DataDouble a1 b1 c1) (DataString a2 b2 c2) = position (zip (map makepoint [(-0.85,0),(0,-0.85),(0,0)]) [text a1 # fontSizeL 0.1 # rotateBy (1/4), text a2 # fontSizeL 0.1, (verticksD (createDoublelist c1 doubleparts) <> horizonsS c2)])
coordCartesian (DataString a1 b1 c1) (DataDouble a2 b2 c2) = position (zip (map makepoint [(-0.85,0),(0,-0.85),(0,0)]) [text a1 # fontSizeL 0.1 # rotateBy (1/4), text a2 # fontSizeL 0.1, (verticksS c1 <> horizonsD (createDoublelist c2 doubleparts))])
coordCartesian (DataString a1 b1 c1) (DataString a2 b2 c2) = position (zip (map makepoint [(-0.85,0),(0,-0.85),(0,0)]) [text a1 # fontSizeL 0.1 # rotateBy (1/4), text a2 # fontSizeL 0.1, (verticksS c1 <> horizonsS c2)])

verticksD :: [String] -> DiaR2
verticksD ys = position (zip ([makepoint (-0.52,-0.5)]) [(mconcat [ text t # fontSizeL 0.025 # moveTo ((-0.14)^&(y*1.0)) | (y,t) <- ylist]) <> (mconcat [fromVertices [0.0^&(y*1.0), 0.02^&(y*1.0)] | (y,t) <- ylist])] )
        where ylist = zip [(fromIntegral d)/(fromIntegral ((length ys)-1)) | d <- [0..((length ys) - 1)]] ys

verticksS :: [String] -> DiaR2
verticksS ys = position (zip ([makepoint (-0.52,-0.5)]) [(mconcat [ text t # fontSizeL 0.025 # moveTo ((-0.14)^&(y*1.0)) | (y,t) <- ylist]) <> (mconcat [fromVertices [0.0^&(y*1.0), 0.02^&(y*1.0)] | (y,t) <- ylist])] ) 
        where ylist = zip [(fromIntegral d)/(fromIntegral ((length ys)+1)) | d <- [1..(length ys)]] ys

horizonsD :: [String] -> DiaR2
horizonsD xs = position (zip ([makepoint (-0.5,-0.52)]) [(mconcat [ text t # fontSizeL 0.025  # rotateBy (1/4) # moveTo ((x*1.0)^&(-0.14)) | (x,t) <- xlist]) <> (mconcat [fromVertices [(x*1.0)^&0.0, (x*1.0)^&0.02] | (x,t) <- xlist])] )
        where xlist = zip [(fromIntegral d)/(fromIntegral ((length xs)-1)) | d <- [0..((length xs) - 1)]] xs

horizonsS :: [String] -> DiaR2
horizonsS xs = position (zip ([makepoint (-0.5,-0.52)]) [(mconcat [ text t # fontSizeL 0.025  # rotateBy (1/4) # moveTo ((x*1.0)^&(-0.14)) | (x,t) <- xlist]) <> (mconcat [fromVertices [(x*1.0)^&0.0, (x*1.0)^&0.02] | (x,t) <- xlist])] )
        where xlist = zip [(fromIntegral d)/(fromIntegral ((length xs)+1)) | d <- [1..(length xs)]] xs

scatterplot :: [(Double, Double, Double, Colour Double)] -> DiaR2
scatterplot a = position (zip (map makepoint [(y,x)|(x,y,_,_)<-a]) [(circle size #fc fillcolor) |(_,_,size,fillcolor) <-a])

createD :: DataFrame -> [Double]
createD (DataDouble _ xs (a,b)) = [(x-a)/(b-a)| x<-xs]
createD (DataString _ xs ds) = [(relate x (zip ds [1.0..]))/fromIntegral  ((length ds)+ 1)| x<-xs]
	
relate :: Eq a => a -> [(a, b)] -> b
relate c = snd.head.dropWhile ((/=c).fst)

joinE :: [Double] -> [Double] -> [Double] -> [Colour Double] -> [(Double, Double, Double, Colour Double)]
joinE x y z w = [(x!!i, y!!i, z!!i, w!!i)| i <- [0..((length x) - 1)]]

example :: DiaR2
example = foogc1 <> foocb <> (square 1 #fc white) <> (square 2 #fc grey)

fooga :: DiaR2
fooga = scatterplot (joinE (createD sepalWidth) (createD sepalLength) (repeat 0.01) (repeat black))# translate (r2 (-0.5, -0.5))

foogb :: DiaR2
foogb = scatterplot (joinE (createD sepalWidth) (createD sepalLength) (repeat 0.01) (foo3))# translate (r2 (-0.5, -0.5))

foogc :: DiaR2
foogc = scatterplot (joinE (createD sepalWidth) (createD sepalLength) foo4 foo3)# translate (r2 (-0.5, -0.5))

fooca :: DiaR2
fooca = coordCartesian sepalWidth sepalLength

foocb :: DiaR2
foocb = coordCartesian petalWidth petalLength

foogc1 :: DiaR2
foogc1 = scatterplot (joinE (createD petalWidth) (createD petalLength) foo5 foo3)# translate (r2 (-0.5, -0.5))

foo3 = mapdisContinous species [white, blue, red]
foo4 = mapdisContinous typeall [0.01, 0.011, 0.012, 0.013, 0.014, 0.015]
foo5 = mapContinous sepalWidth (0.01,0.015)

mapdisContinous :: DataFrame -> [a] -> [a]
mapdisContinous (DataString _ xs ds) map = [ relate x (zip ds map)| x<-xs]
mapdisContinous (DataDouble _ xs ds) map = error "something"

mapContinous :: DataFrame -> (Double, Double) -> [Double]
mapContinous (DataString _ xs ds) (a,b) = error "something"
mapContinous (DataDouble _ xs (d1, d2)) (a,b) = [a + ((x*(b-a))/(d2-d1)) | x<-xs]

main = mainWith example
