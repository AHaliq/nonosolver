{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib 
import System.Environment
import System.IO
import Data.List.Split

import Control.Applicative ((<|>))
import Snap.Core
import Snap.Http.Server

main :: IO ()
main = httpMain

httpMain :: IO ()
httpMain = quickHttpServe $
    ifTop (writeText "Hello World")
    <|> route [("/ping", writeText "Ping"), ("echo/:echoparam", echoHandler)]
    <|> writeText "Bad Path"

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
        writeBS param

fileMain :: IO ()
fileMain = do
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
    