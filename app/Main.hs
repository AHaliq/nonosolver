{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib 
import Parse
import System.Environment
import System.IO
import Data.List.Split
import Data.ByteString.Char8 (unpack, pack)

import Control.Applicative ((<|>))
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

main :: IO ()
main = httpMain

httpMain :: IO ()
httpMain = quickHttpServe $
    --ifTop (writeText "Hello World")
    ifTop (serveFile "site/index.html")
    <|> route
    [ ("/site", serveDirectory "site")
    , ("/solve", method POST solveHandler)]
    <|> writeText "Bad Path"

solveHandler :: Snap ()
solveHandler = do
    param <- getPostParam "text"
    maybe (writeBS "must specify solve/param in URL")
        (\x -> writeBS $ pack $ solveStr (solveJSON) $ formatNewLine $ unpack x) $ param

fileMain :: IO ()
fileMain = do
    getArgs >>= (\x -> if null x
        then do
            hintR <- getDimHints
            hintC <- getDimHints
            putStrLn (solveString hintR hintC)
        else do
            txt <- readFile (head x) >>= (\x -> return $ solveStr (solveString) x)
            putStrLn txt)

formatNewLine :: String -> String
formatNewLine ('n':'n':xs) = '\n' : formatNewLine xs
formatNewLine (x:xs) = x : formatNewLine xs
formatNewLine "" = ""

solveStr :: ([[Int]] -> [[Int]] -> String) -> String -> String
solveStr s x = if length li >= 2 then s (li !! 0) (li !! 1) else "bad puzzle input"
    where
        li :: [[[Int]]]
        li = map (\y -> map (\z -> map read $ words z) $ lines y) $ splitOn "e\n" x

getDimHints :: IO [[Int]]
getDimHints = do
    str <- getLine
    if str == "e"
    then return []
    else ((map read $ words str):) <$> getDimHints
    