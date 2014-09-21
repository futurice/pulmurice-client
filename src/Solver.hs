module Solver where

import Text.Read

solveEcho :: String -> String
solveEcho = id

solveSquare :: Integer -> String
solveSquare x = show (x * x)

solve :: String -> String -> Maybe String
solve "echo"   input = Just (solveEcho input)
solve "square" input = solveSquare `fmap` readMaybe input
solve _ _            = Nothing
