-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  2014 © Futurice OY, Oleg Grenrus
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability   :  experimental
-- Portability :  non-portable (GHC only)
--
-- Pulmurice client.
--
----------------------------------------------------------------------------
module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Aeson as A
import Data.Maybe
import Data.Text as T (Text, unpack)
import Data.Word
import Network.HTTP.Client
import Network.HTTP.Types.Method
import System.IO.Unsafe (unsafePerformIO)

import Pulmurice.Common.Uniq
import Pulmurice.Common.Message

import Options
import Solver

manager :: Manager
manager = unsafePerformIO $ newManager defaultManagerSettings

sendMessage :: String -> ReqMsg -> IO ResMsg
sendMessage endPointHost reqMsg = do
  initReq <- parseUrl endPoint
  let reqBody = RequestBodyLBS $ A.encode reqMsg
  let req = initReq { requestBody = reqBody, method = methodPost }
  body <- responseBody <$> httpLbs req manager
  return $ fromMaybe (ErrorResMsg "can't decode response message") (A.decode body)
     where endPoint = "http://" ++ endPointHost ++ "/api"

handleRest :: ResMsg -> IO ()
handleRest (ErrorResMsg msg) = putStrLn $ "Error: " ++ msg
handleRest resMsg            = putStrLn $ "Error: got unexpected message -- " ++ show resMsg

echo :: String -> Text -> IO ()
echo endPointHost message = do
 resMsg <- sendMessage endPointHost $ EchoReqMsg message
 case resMsg of
   EchoResMsg message' -> putStrLn $ "Echo response: " ++ T.unpack message'
   _                   -> handleRest resMsg

signup :: String -> Text -> Text -> IO ()
signup endPointHost teamName email = do
  resMsg <- sendMessage endPointHost $ SignupReqMsg teamName email
  case resMsg of
    SignupResMsg -> putStrLn "signup received, we will send a verification email shortly."
    _            -> handleRest resMsg

padLeft :: Int -> String -> String
padLeft n str = str ++ replicate (max 0 $ n - length str) ' '

puzzles :: String -> IO ()
puzzles endPointHost = do
  resMsg <- sendMessage endPointHost PuzzlesReqMsg
  case resMsg of
    PuzzlesResMsg ps -> forM_ ps $ \(name, ingress) ->
      putStrLn $ padLeft 20 name ++ " " ++ T.unpack ingress
    _ -> handleRest resMsg

listOpenPuzzles :: String -> Uniq -> IO ()
listOpenPuzzles endPointHost teamToken = do
  resMsg <- sendMessage endPointHost $ ListReqMsg teamToken
  case resMsg of
    ListResMsg ps -> do
      putStrLn $ "There are " ++ show (length ps) ++ " open puzzles"
      forM_ ps $ \(hash, name, diff) ->
        putStrLn $ show hash ++ " " ++ padLeft 20 name ++ " " ++ show diff
    _ -> handleRest resMsg

printPuzzle :: Uniq -> String -> Text -> String -> IO ()
printPuzzle puzzleId name desc input = do
  putStrLn "I don't know how to solve the puzzle :("
  putStrLn $ "Puzzle " ++ name ++ ": " ++ show puzzleId
  putStrLn $ T.unpack desc
  putStrLn "Input: "
  putStrLn input

tryToSolve :: String -> Uniq -> Uniq -> String -> Text -> String -> IO ()
tryToSolve endPointHost teamToken puzzleId name desc input =
  case solve name input of
    Nothing     -> printPuzzle puzzleId name desc input
    Just output -> do
      resMsg <- sendMessage endPointHost $ SolveReqMsg teamToken puzzleId output
      case resMsg of
        SolveResMsg -> putStrLn $ "Puzzle " ++ name ++ " solved -- " ++ show puzzleId
        _ -> handleRest resMsg

newPuzzle :: String -> Uniq -> String -> Word16 -> IO ()
newPuzzle endPointHost teamToken puzzleName diff = do
  resMsg <- sendMessage endPointHost $ NewReqMsg teamToken puzzleName diff
  case resMsg of
    NewResMsg puzzleId desc input -> tryToSolve endPointHost teamToken puzzleId puzzleName desc input
    _ -> handleRest resMsg

showPuzzle :: String -> Uniq -> Uniq -> IO ()
showPuzzle endPointHost teamToken puzzleId = do
  resMsg <- sendMessage endPointHost $ ShowReqMsg teamToken puzzleId
  case resMsg of
    ShowResMsg _puzzleId name desc input -> printPuzzle puzzleId name desc input
    _ -> handleRest resMsg

solvePuzzle :: String -> Uniq -> Uniq -> IO ()
solvePuzzle endPointHost teamToken puzzleId = do
  resMsg <- sendMessage endPointHost $ ShowReqMsg teamToken puzzleId
  case resMsg of
    ShowResMsg _puzzleId name desc input -> tryToSolve endPointHost teamToken puzzleId name desc input
    _ -> handleRest resMsg

solveAllPuzzles :: String -> Uniq -> String -> IO ()
solveAllPuzzles endPointHost teamToken puzzleName = do
  resMsg <- sendMessage endPointHost $ ListReqMsg teamToken
  case resMsg of
    ListResMsg ps -> forM_ ps $ \(puzzleId, puzzleName', _diff) -> do
      putStrLn $ "Solving " ++ show puzzleId
      if puzzleName == puzzleName'
         then solvePuzzle endPointHost teamToken puzzleId
         else putStrLn $ "  - skipped, is of type: " ++ puzzleName'
    _ -> handleRest resMsg

main :: IO ()
main = do
  (endPointHost, cmd) <- parseOptions
  case cmd of
    CmdEcho message                    -> echo endPointHost message
    CmdHelp                            -> putStrLn helpString
    CmdPuzzles                         -> puzzles endPointHost
    CmdSignup teamName email           -> signup endPointHost teamName email
    CmdList token                      -> listOpenPuzzles endPointHost token
    CmdNew token puzzleName difficulty -> newPuzzle endPointHost token puzzleName difficulty
    CmdShow token puzzleId             -> showPuzzle endPointHost token puzzleId
    CmdSolve token puzzleId            -> solvePuzzle endPointHost token puzzleId
    CmdSolveAll token puzzleName       -> solveAllPuzzles endPointHost token puzzleName
