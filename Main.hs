{-# LANGUAGE TemplateHaskell #-}
import System.Environment
import System.Directory
import System.FilePath.Posix
import System.IO

import qualified Options.Applicative as Opt
import Options.Applicative

import qualified Data.ByteString as BS
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Time

import Text.Regex.PCRE

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens

log :: MonadIO m => String -> m ()
log str = liftIO $ hPutStrLn stderr str

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

type StatsM = ResourceT IO

data LogFile = LogFile Day FilePath
               deriving (Show)
getFilesMatching :: FilePath -> (String -> Bool) -> IO [FilePath]
getFilesMatching path pattern = do
  dirContents <- getDirectoryContents path
  return $ filter pattern dirContents

getDirectoriesMatching :: FilePath -> (String -> Bool) -> IO [FilePath]
getDirectoriesMatching path pattern = do
  matching <- getFilesMatching path pattern 
  filterM (doesDirectoryExist . combine path) matching


logFilesSource :: FilePath -> Source StatsM LogFile
logFilesSource logs = logYearsSource logs $= logMonthsConduit $= logDaysConduit

logYearsSource :: FilePath -> Source StatsM (Integer, FilePath)
logYearsSource logs = do
  years <- liftIO $ getDirectoriesMatching logs (=~ "\\d{4}")
  log $ "years " ++ show years
  CL.sourceList $ map (\year -> (read year, combine logs year)) years

logMonthsConduit :: Conduit (Integer, FilePath) StatsM (Integer, Int, FilePath)
logMonthsConduit = do
  m <- await
  case m of
    Nothing -> log "All years processed"
    Just (year, yearPath) -> do
      log $ "Processing year " ++ show year
      months <- liftIO $ getDirectoriesMatching yearPath (=~ "0[1-9]|1[0-2]")
      CL.sourceList $ map (\month -> (year, read month, combine yearPath month)) months
      logMonthsConduit
  

logDaysConduit :: Conduit (Integer, Int, FilePath) StatsM LogFile
logDaysConduit = do
  m <- await
  case m of
    Nothing -> log "All months processed"
    Just (year, month, monthPath) -> do
      log $ "Processing month " ++ show year ++ "/" ++ show month
      days <- liftIO $ getFilesMatching monthPath (=~ "([0-2][0-9]|3[0-1])\\.txt")
      log $ "days " ++ show days
      CL.sourceList $ map makeLogFile days
      logDaysConduit
      where makeLogFile day =
              LogFile (fromGregorian year month (read . dropExtension $ day))
                      (combine monthPath day)

logFileIrcLinesConduit :: Conduit LogFile StatsM (Day, BS.ByteString)
logFileIrcLinesConduit = do
  m <- await
  case m of
    Nothing -> log "All log files processed"
    Just (LogFile day path) -> do
      toProducer $ mapOutput ((,) day) $ CB.sourceFile path $= CB.lines
      logFileIrcLinesConduit

debugShowSink :: Show a => Sink a StatsM ()
debugShowSink = do
  m <- await
  case m of
    Nothing -> log "debug: done"
    Just a -> do
      log $ "debug: got  " ++ show a
      debugShowSink

stats :: StatsArgs -> IO ()
stats args = do
  let path = args^.logsPath
  putStrLn path
  runResourceT $ logFilesSource path $$ debugShowSink


