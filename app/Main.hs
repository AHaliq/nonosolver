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
import Snap.Util.CORS

import System.Directory
import Text.JSON

main :: IO ()
main = httpMain

httpMain :: IO ()
httpMain = do
    tests <- loadFiles getTestPaths
    quickHttpServe $
    --ifTop (writeText "Hello World")
        ifTop (serveFile "site/index.html")
        <|> route
        [ ("/site", serveDirectory "site")
        , ("/solve", applyCORS defaultOptions $ method POST $ solveHandler)
        , ("/tests", applyCORS defaultOptions $ method GET $ testsHandler tests)]
        <|> writeText "Bad Path"

getTestPaths :: IO [String]
getTestPaths = getDirectoryContents "./site/testcases/" >>= (\xs -> return $ map ("./site/testcases/"++) $ filter (\a -> a /= "." && a /= "..") xs)

loadFiles :: IO [String] -> IO [[String]]
loadFiles x = x
    >>= (\v -> foldl
        (\a' b -> a' >>= (\a -> readFile b >>= (\f -> return $ a++[[b,f]])))
        (return [["",""]]) v)
    >>= (\xs -> return $ tail xs)

solveHandler :: Snap ()
solveHandler = do
    param <- getPostParam "text"
    solver <- getPostParam "solv"
    let s = maybe 1 (\x -> read $ unpack x) solver in
        maybe (writeBS "must specify solve/param in URL")
            (\x -> writeBS $ pack $ solveStr (getSolver s) $ formatNewLine $ unpack x) $ param

getSolver :: Int -> ([[Int]] -> [[Int]] -> String)
getSolver 1 = solveJSON
getSolver 2 = solveOneJSON
getSolver x = solveLineOnlyJSON

testsHandler :: [[String]] -> Snap ()
testsHandler s = do
    writeBS $ pack $ encode $ s

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
    