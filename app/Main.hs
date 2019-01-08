module Main where

import Lib 
import System.Environment
import System.IO
import Data.List.Split

main :: IO ()
main = do
    args <- getArgs
    getArgs >>= (\x -> if null x
        then do
            hintR <- getDimHints
            hintC <- getDimHints
            putStrLn (solve hintR hintC)
        else do
            li <- readFile (head x) >>=
                (\x -> return $ map
                    (\y -> map
                        (\z -> map read $ words z) $
                    lines y) $
                splitOn "e\n" x)
            putStrLn (solve (li !! 0) (li !! 1)))

getDimHints :: IO [[Int]]
getDimHints = do
    str <- getLine
    if str == "e"
    then return []
    else ((map read $ words str):) <$> getDimHints
    