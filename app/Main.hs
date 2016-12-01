module Main where

import Lib

import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch ("read" : file : _) = fmap printZone <$> readZone file >>= printEither
dispatch  _                  = help

printEither :: Either String String -> IO ()
printEither (Left msg) = putStrLn msg
printEither (Right s)  = putStrLn s

help :: IO ()
help = do
  putStrLn "read <zone> : read a zone file"
  putStrLn "help        : this message"
