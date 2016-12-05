module Main where

import Lib

import System.Environment (getArgs)

help :: IO ()
help = do
  putStrLn "clean <zone> : read a zone file, - or none for standard input"
  putStrLn "help         : this message"

main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch ["clean"]            = cleanZone stdin
dispatch ("clean" : "-"  : _) = cleanZone stdin
dispatch ("clean" : file : _) = cleanZone file
dispatch  _                  = help

cleanZone :: FilePath -> IO ()
cleanZone file = fmap printZone <$> readZone file >>= printEither

printEither :: Either String String -> IO ()
printEither (Left msg) = putStrLn msg
printEither (Right s)  = putStrLn s

stdin :: FilePath
stdin = "/dev/stdin"
