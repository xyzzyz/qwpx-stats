{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
import System.Environment
--import System.Directory

import qualified Options.Applicative as Opt
import Options.Applicative

import Data.Conduit
import Data.Conduit.Binary
import Data.Time

import Control.Applicative
import Control.Lens

data StatsArgs = StatsArgs {
  _logsPath :: String
  } deriving Show

data LogFile = LogFile Day FilePath

makeLenses ''StatsArgs

argsParser :: Opt.Parser StatsArgs
argsParser = StatsArgs <$>
             strOption (long "logs"
                        <> metavar "LOGS"
                        <> help "Directory containing logs")

stats :: StatsArgs -> IO ()
stats args = do
  let path = args^.logsPath
  putStrLn path

main = execParser args >>= stats
  where
    args = info (helper <*> argsParser)
      ( fullDesc
     <> progDesc "Generate statistics from IRC logs"
     <> header "qwpx-stats -- generate statistics" )

