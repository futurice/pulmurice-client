module Solver where

solveEcho :: String -> String
solveEcho = id

solve :: String -> String -> Maybe String
solve "echo" input = Just (solveEcho input)
solve _ _          = Nothing
