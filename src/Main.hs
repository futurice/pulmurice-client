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
module Main where

import Control.Applicative
import Control.Monad
import Data.Aeson as A
import Data.Maybe
import Data.Text as T (Text, pack, unpack)
import Data.Word
import Network.HTTP.Client
import Network.HTTP.Types.Method
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

import Pulmurice.Common.Uniq
import Pulmurice.Common.Message

import Solver

endPointHost :: String
endPointHost = "floating-plains-8402.herokuapp.com"
-- endPointHost = "localhost:8080"

help :: String
help = unlines
  [ "pulmurice"
  , ""
  , "Available commands"
  , "  - help"
  , "  - signup <team-name> <email>              Create a new account"
  , "  - [token] puzzles                         List different puzzle types"
  , "  - <token> list                            List open puzzles"
  , "  - <token> new <puzzle-name> <difficulty>  Creates new puzzle, tries to auto-solve"
  , "  - <token> show <puzzle-hash>              Show the puzzle"
  , "  - <token> solve <puzzle-hash>             Tries to solve the puzzle"
  ]

manager :: Manager
manager = unsafePerformIO $ newManager defaultManagerSettings

endPoint :: String
endPoint = "http://" ++ endPointHost ++ "/api"

sendMessage :: ReqMsg -> IO ResMsg
sendMessage reqMsg = do
  initReq <- parseUrl endPoint
  let reqBody = RequestBodyLBS $ A.encode reqMsg
  let req = initReq { requestBody = reqBody, method = methodPost }
  body <- responseBody <$> httpLbs req manager
  return $ fromMaybe (ErrorResMsg "can't decode response message") (A.decode body)

handleRest :: ResMsg -> IO ()
handleRest (ErrorResMsg msg) = putStrLn $ "Error: " ++ msg
handleRest resMsg            = putStrLn $ "Error: got unexpected message -- " ++ show resMsg

echo :: Text -> IO ()
echo message = do
 resMsg <- sendMessage $ EchoReqMsg message
 case resMsg of
   EchoResMsg message' -> putStrLn $ "Got: " ++ T.unpack message'
   _                   -> handleRest resMsg

signup :: Text -> Text -> IO ()
signup team email = do
  resMsg <- sendMessage $ SignupReqMsg team email
  case resMsg of
    SignupResMsg -> putStrLn "signup received, we will send a verification email shortly."
    _            -> handleRest resMsg

padLeft :: Int -> String -> String
padLeft n str = str ++ replicate (max 0 $ n - length str) ' '

puzzles :: IO ()
puzzles = do
  resMsg <- sendMessage PuzzlesReqMsg
  case resMsg of
    PuzzlesResMsg ps -> forM_ ps $ \(name, ingress) ->
      putStrLn $ padLeft 20 name ++ " " ++ T.unpack ingress
    _ -> handleRest resMsg

withUniq :: String -> (Uniq -> IO ()) -> IO ()
withUniq t action = case fromString t of
                       Nothing    -> putStrLn "can't parse team token"
                       Just token -> action token

listOpenPuzzles :: Uniq -> IO ()
listOpenPuzzles token = do
  resMsg <- sendMessage $ ListReqMsg token
  case resMsg of
    ListResMsg ps -> do
      putStrLn $ "There are " ++ show (length ps) ++ " open puzzles"
      forM_ ps $ \(hash, name, diff) ->
        putStrLn $ show hash ++ " " ++ padLeft 20 name ++ " " ++ show diff
    _ -> handleRest resMsg

printPuzzle :: Uniq -> String -> Text -> String -> IO ()
printPuzzle puzzleId name desc input = do
      putStrLn "I Don't know how to solve the puzzle :("
      putStrLn $ "Puzzle " ++ name ++ ": " ++ show puzzleId
      putStrLn $ T.unpack desc
      putStrLn "Input: "
      putStrLn input

tryToSolve :: Uniq -> Uniq -> String -> Text -> String -> IO ()
tryToSolve token puzzleId name desc input =
  case solve name input of
    Nothing     -> printPuzzle puzzleId name desc input
    Just output -> do
      resMsg <- sendMessage $ SolveReqMsg token puzzleId output
      case resMsg of
        SolveResMsg -> putStrLn $ "Puzzle " ++ name ++ " solved -- " ++ show puzzleId
        _ -> handleRest resMsg

newPuzzle :: String -> Word16 -> Uniq -> IO ()
newPuzzle name diff token = do
  resMsg <- sendMessage $ NewReqMsg token name diff
  case resMsg of
    NewResMsg puzzleId desc input -> tryToSolve token puzzleId name desc input
    _ -> handleRest resMsg

showPuzzle :: Uniq -> Uniq -> IO ()
showPuzzle token puzzleId = do
  resMsg <- sendMessage $ ShowReqMsg token puzzleId
  case resMsg of
    ShowResMsg _puzzleId name desc input -> printPuzzle puzzleId name desc input
    _ -> handleRest resMsg

solvePuzzle :: Uniq -> Uniq -> IO ()
solvePuzzle token puzzleId = do
  resMsg <- sendMessage $ ShowReqMsg token puzzleId
  case resMsg of
    ShowResMsg _puzzleId name desc input -> tryToSolve token puzzleId name desc input
    _ -> handleRest resMsg

toBoundedIntegral :: (Integral a, Bounded a) => Integer -> a
toBoundedIntegral integer
  | integer < toInteger minB = minB
  | integer > toInteger maxB = maxB
  | otherwise                = fromIntegral integer
  where maxB    = maxBound -- These forces types to unify
        minB    = minBound


readBoundedIntegral :: (Integral a, Bounded a) => String -> a
readBoundedIntegral = toBoundedIntegral . read

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ "echo", message ]         -> echo (T.pack message)
    [ "puzzles" ]               -> puzzles
    [ _token, "puzzles" ]       -> puzzles
    [ "signup", team, email ]   -> signup (T.pack team) (T.pack email)
    [ token, "list" ]           -> withUniq token listOpenPuzzles
    [ token, "new", name, diff] -> withUniq token $ newPuzzle name (readBoundedIntegral diff)
    [ token, "show", puzzleId]  -> withUniq token $ \t -> withUniq puzzleId $ \p -> showPuzzle t p
    [ token, "solve", puzzleId] -> withUniq token $ \t -> withUniq puzzleId $ \p -> solvePuzzle t p
    _ -> putStrLn help
