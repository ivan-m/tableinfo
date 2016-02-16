{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : Main
   Description : Try and determine structure of messy tables
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Main where

import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as B
import           Data.String
import           Options.Applicative

main :: IO ()
main = return ()

--------------------------------------------------------------------------------
-- Argument handling

data Arguments = Arguments { dbConnStr :: ByteString
                           , tableName :: String
                           }
               deriving (Eq, Ord, Show, Read)

parseArguments :: ParserInfo Arguments
parseArguments = info (helper <*> prs) fullDesc
  where
    prs = Arguments <$> parseDB
                    <*> parseTbl

    parseDB = txtOpt [ long "dbconn"
                     , short 'c'
                     , metavar "DB_CONNECTION_STRING"
                     , value "postgresql:///"
                     , showDefault
                     , help "PostgreSQL database connection string"
                     ]

    parseTbl = strArgument (help "TABLE")

txtOpt :: (IsString s) => [Mod OptionFields String] -> Parser s
txtOpt = fmap fromString . strOption . mconcat

--------------------------------------------------------------------------------
