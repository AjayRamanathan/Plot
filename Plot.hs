{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Text
import Diagrams.Core.Points

type Dia = Diagram B R2

type DataInt = ([String], [Double], [Int])
type DataString = ([String], [String], [String])

city :: DataString
city = (["city"],["Tokyo","Mumbai","New York","Los Angeles","Paris","Moscow","London","Chicago","Toronto","Berlin","Melbourne","Paris","Moscow","Toronto","Berlin","Melbourne"],["Tokyo","Mumbai","New York","Los Angeles","Paris","Moscow","London","Chicago","Toronto","Berlin","Melbourne"])

group :: DataString
group = (["group"],["World","World","USA","USA","World","World","World","USA","World","World","World","USA","USA","USA","USA","USA"],["USA", "World"])

team :: DataString
team = (["Team"],["C","B","A","C","B","A","B","B","B","A","C","C","C","B","A","A"],["A", "B", "C"])

pop2000 :: DataInt
pop2000 = (["Population"],[26.2,18.4,16.7,14.3,9.6,7.2,7.1,6.4,5.3,4.8,2.9,2.1,1.7,1.3,0.8,0.5],[0,4..32])

cross :: DataString -> DataString -> [DataString]
cross ([a],b,c) ([x],y,z) = [([a,x,(z!!t)],(resulting b y (z!!t)),c) |t <- [0..((length z) - 1)]]

resulting :: [String] -> [String] -> String -> [String]
resulting xs ys c = [if ((ys!!t) == c) then (xs!!t) else "None"|t <-[0..((length xs) - 1)]]

nest :: DataString -> DataString -> [DataString]
nest ([a],b,c) ([x],y,z) = [([a,x,(z!!t)],(resulting b y (z!!t)),(removeN (unique (resulting b y (z!!t))))) |t <- [0..((length z) - 1)]]

has :: (Eq a) => [a] -> a -> Bool
has [] _ = False
has (x:xs) a
  | x == a    = True
  | otherwise = has xs a

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
  | has xs x  = unique xs
  | otherwise = x : unique xs

removeN :: [String] -> [String]
removeN xs = [x |x <- xs, x /= "None"]

createpoints :: [(Double,String)] -> [String]-> [Int] -> [(Double,Double)]
createpoints a b c = [(z,(fromIntegral t+1)/(fromIntegral (length b) + 1))|(z,r)<-rN, t <- [0..((length b) - 1)], r == (b!!t) ]
                     where rN = [(x/(fromIntegral (c!!0)),y) |(x,y)<- a, y /= "None"]

plotfunction :: [DataString] -> DataInt -> Dia
plotfunction xs ys = position (zip (map makepoint [(0,0),((fromIntegral(length xs)-1),0)]) [( hcat' (with & sep .~ 0.25 ) [plotSingle x ys | x<-xs] ), (rect (2*fromIntegral(length xs)) 2 #fc grey)])

plotSingle :: DataString -> DataInt -> Dia
plotSingle (a,b,c) ([x],y,z) = if (length a) == 1 then plotmake [x,a!!0] c (map show z) (createpoints [(y!!t,b!!t) | t <- [0..((length y) - 1)]] c [last(z)]) else plotmake [x,a!!0,a!!2] c (map show z) (createpoints [(y!!t,b!!t) | t <- [0..((length y) - 1)]] c [last(z)])

makepoint (x,y) = p2 (x,y) 

plotmake :: [String] -> [String] -> [String] -> [(Double,Double)] -> Dia
plotmake a b c d = if (length a) == 2 then (plotsimple a b c d) else position (zip (map makepoint [(0,0.75),(0,0)]) [label (a!!2),plotsimple a b c d])

label :: String -> Dia
label a = text a # fontSizeL 0.1 <> rect 1 0.2 #fc lightblue

plotsimple :: [String] -> [String] -> [String] -> [(Double,Double)] -> Dia
plotsimple a b c d = position (zip (map makepoint [(-0.5,-0.5),(0,0)]) [plotpoints d, plotbase a b c] )

plotpoints :: [(Double,Double)] -> Dia
plotpoints a = position (zip (map makepoint [(v,c)|(c,v)<-a]) (repeat dot))

dot :: Dia
dot = circle 0.01 # fc red

plotbase :: [String] -> [String] -> [String]-> Dia
plotbase a b c =  position (zip (map makepoint [(-0.75,0),(0,-0.85),(0,0)]) [text (a!!0)# fontSizeL 0.1 # rotateBy (1/4),text (a!!1)# fontSizeL 0.1,(verticals c <> horizons b)])

verticals :: [String] -> Dia
verticals xs = position (zip ([makepoint (-0.52,-0.5)]) [(mconcat [ text t # fontSizeL 0.035 # moveTo ((-0.04)^&(y*1)) | (y,t) <- ylist]) <> (mconcat [fromVertices [0^&(y*1), 0.02^&(y*1)] | (y,t) <- ylist])] ) `atop` square 1 #fc white 
        where ylist = zip [(fromIntegral d)/(fromIntegral ((length xs)-1)) | d <- [0..((length xs) - 1)]] xs

horizons :: [String] -> Dia
horizons ys = position (zip ([makepoint (-0.5,-0.52)]) [(mconcat [ text t # fontSizeL 0.035  # rotateBy (1/4) # moveTo ((x*1)^&(-0.14)) | (x,t) <- xlist]) <> (mconcat [fromVertices [(x*1)^&0, (x*1)^&0.02] | (x,t) <- xlist])] )
        where xlist = zip [(fromIntegral d)/(fromIntegral ((length ys)+1)) | d <- [1..(length ys)]] ys

example :: Dia
example = plotfunction c1  pop2000

a1 = [city]
b1 = (cross city team)
b2 = (cross city group)
c1 = (nest city team)
c2 = (nest city group)

main = mainWith example



                                                                       
                                       


 
