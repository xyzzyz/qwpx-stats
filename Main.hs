{-# LANGUAGE TemplateHaskell #-}
import System.Environment
import System.Directory
import System.FilePath.Posix

import qualified Options.Applicative as Opt
import Options.Applicative

import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import Data.Time

import Text.Regex.PCRE

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens

data StatsArgs = StatsArgs {
  _logsPath :: FilePath
  } deriving Show

makeLenses ''StatsArgs

argsParser :: Opt.Parser StatsArgs
argsParser = StatsArgs <$>
             strOption (long "logs"
                        <> metavar "LOGS"
                        <> help "Directory containing logs")

main = execParser args >>= stats
  where
    args = info (helper <*> argsParser)
      ( fullDesc
     <> progDesc "Generate statistics from IRC logs"
     <> header "qwpx-stats -- generate statistics" )

type StatsM = IO

data LogFile = LogFile Day FilePath

getDirectoriesMatching :: FilePath -> (String -> Bool) -> IO [FilePath]
getDirectoriesMatching path pattern = do
  dirContents <- getDirectoryContents path
  let matching = filter pattern dirContents
  filterM doesDirectoryExist matching


logFilesSource :: FilePath -> Source StatsM LogFile
logFilesSource logs = logYearsSource logs $= logMonthsConduit $= logDaysConduit

logYearsSource :: FilePath -> Source StatsM (Int, FilePath)
logYearsSource logs = do
  years <- liftIO $ getDirectoriesMatching logs (=~ "\\d{4}")
  CL.sourceList $ map ((,) <$> read <*> combine logs) years

logMonthsConduit :: Conduit (Int, FilePath) StatsM (Int, Int, FilePath)
logMonthsConduit = undefined

logDaysConduit :: Conduit (Int, Int, FilePath) StatsM LogFile
logDaysConduit = undefined

stats :: StatsArgs -> IO ()
stats args = do
  let path = args^.logsPath
  putStrLn path


