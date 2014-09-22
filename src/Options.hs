-----------------------------------------------------------------------------
-- |
-- Module      :  Options
-- Copyright   :  2014 © Futurice OY, Oleg Grenrus
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability   :  experimental
-- Portability :  non-portable (GHC only)
--
-- Options parsing
--
----------------------------------------------------------------------------
module Options (Command(..), parseOptions, helpString) where

import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.Word
import Data.Text as T (Text, pack)
import Options.Applicative
import Options.Applicative.Help (parserHelp)
import System.Environment (lookupEnv)
import Text.Read


import Pulmurice.Common.Uniq

-- Alternative ReadM
instance Alternative ReadM where
  empty = mzero
  (<|>) = mplus

-- (.) for functor
(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
(<.>) f g x = f <$> g x

-- Defaults
endPointHost :: String
endPointHost = "floating-plains-8402.herokuapp.com"

data Command = CmdHelp
             | CmdEcho Text
             | CmdSignup Text Text
             | CmdPuzzles
             | CmdList Uniq
             | CmdNew Uniq String Word16
             | CmdShow Uniq Uniq
             | CmdSolve Uniq Uniq

toBoundedIntegral :: (Integral a, Bounded a) => Integer -> a
toBoundedIntegral integer
  | integer < toInteger minB = minB
  | integer > toInteger maxB = maxB
  | otherwise                = fromIntegral integer
  where maxB    = maxBound -- These forces types to unify
        minB    = minBound

simpleStrArgument :: String -> Parser String
simpleStrArgument mv = argument str (metavar mv)

simpleTextArgument :: String -> Parser Text
simpleTextArgument mv = T.pack <$> simpleStrArgument mv

uniqArgument :: String -> Parser Uniq
uniqArgument mv = argument fromString (metavar mv) 

word16Argument :: String -> Parser Word16
word16Argument mv = argument (toBoundedIntegral <.> readMaybe) (metavar mv)

teamTokenOption :: Maybe Uniq -> Parser Uniq
teamTokenOption teamToken = option fromString (short 't' <> long "team-token" <> metavar "TEAMTOKEN" <> defaultValue)
  where defaultValue = case teamToken of
                         Nothing -> mempty
                         Just t  -> value t

parserCommand :: Maybe Uniq -> Parser Command
parserCommand teamToken = subparser $ mconcat $ map simpleCommand
  [ ("help",    pure CmdHelp,  "Show help")
  , ("echo",    parserEcho,    "Ping the server")
  , ("puzzles", parserPuzzles, "List all available puzzle types")
  , ("signup",  parserSignup,  "Sign up")
  , ("list",    parserList,    "List all open puzzle challenges")
  , ("new",     parserNew,     "Request new puzzle challenge, will try to auto-solve")
  , ("show",    parserShow,    "Show puzzle challenge")
  , ("solve",   parserSolve,   "Solve puzzle challenge")
  ] 
  where simpleCommand (cmd, parser, desc) = command cmd (info parser (progDesc desc))
        parserPuzzles        = pure CmdPuzzles
        parserEcho           = CmdEcho <$> simpleTextArgument "message"
        parserSignup         = CmdSignup <$> simpleTextArgument "team-name" <*> simpleTextArgument "email-address"
        parserList           = CmdList <$> teamTokenOption teamToken
        parserNew            = CmdNew <$> teamTokenOption teamToken <*> simpleStrArgument "puzzle-name" <*> word16Argument "difficulty"
        parserShow           = CmdShow <$> teamTokenOption teamToken <*> uniqArgument "puzzle-id"
        parserSolve          = CmdSolve <$> teamTokenOption teamToken <*> uniqArgument "puzzle-id"

parserHost :: String -> Parser String
parserHost defaultHost = strOption $ mconcat
  [ long "host"
  , short 'H'
  , metavar "HOST"
  , help "pulmurice server host to connect to"
  , value defaultHost
  ]

parserOptions :: String -> Maybe Uniq -> Parser (String, Command)
parserOptions defaultHost teamToken = (,) <$>  (parserHost defaultHost) <*> parserCommand teamToken

options :: String -> Maybe Uniq -> ParserInfo (String, Command)
options defaultHost teamToken = info (helper <*> parserOptions defaultHost teamToken) $ mconcat
  [ fullDesc
  , header "pulmurice - a client for pulmurice programming puzzle game"
  ]

endPointHostIO :: IO String
endPointHostIO = fromMaybe endPointHost <$> lookupEnv "PULMURICE_HOST"

teamTokenIO :: IO (Maybe Uniq)
teamTokenIO = (>>= fromString) <$> lookupEnv "PULMURICE_TEAMTOKEN" 

-- | exec options, return host and command
parseOptions :: IO (String, Command)
parseOptions = do
  teamToken   <- teamTokenIO
  defaultHost <- endPointHostIO
  execParser $ options defaultHost teamToken

helpString :: String
helpString = show $ parserHelp (prefs mempty) $ parserOptions "" Nothing
