module Parse 
    ( solveString,
      solveJSON,
      solveOneJSON,
      solveExpOneJSON,
      solveLineOnlyJSON,
      matToList
    ) where

import Lib
import Text.JSON
import qualified Data.Matrix as M
import qualified Data.Vector as V

solveString :: [[Int]] -> [[Int]] -> String
solveString hr hc =
    let solns = solve hr hc in
        if null solns
            then "Invalid Puzzle"
            else foldl (\a b -> a ++ "\n" ++ b) "" $ map M.prettyMatrix $ solns

solveJSON :: [[Int]] -> [[Int]] -> String
solveJSON hr hc = encode $ map matToList $ solve hr hc

solveOneJSON :: [[Int]] -> [[Int]] -> String
solveOneJSON hr hc = encode $ maybe [] (\x -> [matToList x]) $ solveOne hr hc

solveExpOneJSON :: [[Int]] -> [[Int]] -> String
solveExpOneJSON hr hc = encode $ maybe [] (\x -> [matToList x]) $ solveExpOne hr hc

solveLineOnlyJSON :: [[Int]] -> [[Int]] -> String
solveLineOnlyJSON hr hc = encode $ maybe [] (\x -> [matToList x]) $ solveLineOnly hr hc

matToList :: Show a => M.Matrix a -> [[String]]
matToList m = map (\x -> map show $ V.toList $ M.getRow x m) [1..(M.nrows m)]