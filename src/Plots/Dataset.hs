{-# LANGUAGE TemplateHaskell #-}

module Plots.Dataset
    ( 
    -- *data stuff
    DataFrame (..)
    , iris
    
    , sepalLength
    , sepalWidth
    , petalLength
    , petalWidth
    , species
    , typeall 

    , datadoubleA
    , datadoubleB
    , datadoubleB2
    , datastringA
    , datastringB
    ) where

import Data.Default
import Data.List

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

--sets of 10--

datadoubleA  = DataDouble "double 1"  [0.1, 1.2, 2.3, 3.5, 5.1, 5.8, 6.4, 7.5, 8.5, 9.1, 9.6] (0.0,10.0)
datadoubleB  = DataDouble "double 2"  [111, 140, 150, 155, 167, 191, 200, 211, 233, 250, 270] (100.0,300.0)
datadoubleB2 = DataDouble "double 22" [130, 160, 170, 185, 197, 207, 212, 236, 254, 279, 281] (100.0,300.0)
datastringA  = DataString "string 1"  ["typeA","typeA","typeA","typeB","typeB","typeC","typeB" ,"typeB" ,"typeC","typeC","typeC"] ["typeA","typeB","typeC"]
datastringB  = DataString "string 2"  ["A","B","C","D","E","A","B","C","D","E","E"] ["A","B","C","D","E"]
