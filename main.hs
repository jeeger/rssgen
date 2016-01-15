{-# LANGUAGE TemplateHaskell #-}
import System.IO
import System.Exit
import LogParser
import Data.Maybe
import Data.List
import Data.ConfigFile
import Text.ParserCombinators.Parsec.Error
import Control.Lens
import Control.Monad.Except
import Control.Monad
import Data.String
import Text.RSS
import Network.URI

-- TODO: Generate RSS output
data Config = Config {
  _fileSourcePath :: String,
  _fileDestURL :: String,
  _rssFileLocation :: String,
  _rssTitle :: String,
  _rssLink :: String,
  _rssDescription :: String
  } deriving (Show)

$(makeLenses ''Config)


type RSSgenError = Either (CPErrorData, String) Text.ParserCombinators.Parsec.Error.ParseError
type RSSgenErrorIO = ExceptT RSSgenError IO

readConfig:: FilePath -> ExceptT (CPErrorData, String) IO Config
readConfig p = do cp <- join $ liftIO $ readfile emptyCP p
                  fs <- get cp "DEFAULT" "filesource"
                  fd <- get cp "DEFAULT" "filedest"
                  rssloc <- get cp "DEFAULT" "rssfile"
                  rssdesc <- get cp "DEFAULT" "rssdesc"
                  rsstitle <- get cp "DEFAULT" "rsstitle"
                  rsslink <- get cp "DEFAULT" "rsslink"
                  return Config { _fileSourcePath = fs,
                                  _fileDestURL = fd,
                                  _rssFileLocation = rssloc,
                                  _rssTitle = rsstitle,
                                  _rssLink = rsslink,
                                  _rssDescription = rssdesc
                                }

-- Replace all non-overlapping occurrences of old with new.
replace:: [Char] -> [Char] -> [Char] -> [Char]
replace old new [] = []
replace old new str = if old `isPrefixOf` str then
                        new ++ replace old new (drop (length old) str)
                      else
                        (head str) : (replace old new (tail str))

filterEntries:: LogEntries -> LogEntries
filterEntries =
  map (over description (replace "\nINCLUDE_RSS" ""))
  . filter (views description (isInfixOf "\nINCLUDE_RSS"))


processEntries:: LogEntries -> Config -> RSS
processEntries entries config = RSS "thenybble.de" (fromJust (parseURI "https://thenybble.de")) "thenybble.de RSS feed" [] []

runMain:: String -> RSSgenErrorIO ()
runMain confpath = do conf <- withExceptT (\e -> Left e) $ readConfig confpath
                      lines <- liftIO getContents
                      entries <- withExceptT (\e -> Right e) $ ExceptT $ return $ parseLog lines "standard input"
                      result <- return $ processEntries (filterEntries entries) conf
                      return ()
                      
main = do result <- runExceptT $ runMain "rssgen.conf"
          case result of
            Left err -> print err >> exitFailure
            Right _ -> exitSuccess
            
