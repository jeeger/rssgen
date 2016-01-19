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
import Paths_rssgen
import Data.Version (showVersion)

-- TODO: Generate RSS output
data Config = Config {
  _fileSourcePath :: String,
  _fileDestURL :: String,
  _rssFileLocation :: String,
  _rssTitle :: String,
  _rssLink :: String,
  _rssCopyright :: String,
  _rssDescription :: String,
  _fromExtension :: String,
  _toExtension :: String
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
                  fromext <- get cp "DEFAULT" "fromext"
                  toext <- get cp "DEFAULT" "toext"
                  copyright <- get cp "DEFAULT" "copyright"
                  return Config { _fileSourcePath = fs,
                                  _fileDestURL = fd,
                                  _rssFileLocation = rssloc,
                                  _rssTitle = rsstitle,
                                  _rssLink = rsslink,
                                  _rssDescription = rssdesc,
                                  _fromExtension = fromext,
                                  _toExtension = toext,
                                  _rssCopyright = copyright
                                }

-- Replace all non-overlapping occurrences of old with new.
replace:: [Char] -> [Char] -> [Char] -> [Char]
replace old new [] = []
replace old new str = if old `isPrefixOf` str then
                        new ++ replace old new (drop (length old) str)
                      else
                        (head str) : (replace old new (tail str))


-- Filter out all entries containing the RSS token, remove RSS token
-- 
filterAndRemoveRSSToken:: LogEntries -> LogEntries
filterAndRemoveRSSToken =
  let rsstoken = "\n\nINCLUDE_RSS" in
  map (over description (replace rsstoken ""))
  . filter (views description (isInfixOf rsstoken))

generateRSS:: Config -> LogEntries -> RSS
generateRSS config entries = RSS
  (view rssTitle config)
  (views rssLink (fromJust . parseURI) config)
  (view rssDescription config)
  [ (views rssCopyright Copyright config), Generator $ "rssgen-" ++ (showVersion version)]
  (map (processEntry config) entries)

-- Find the file with the most added changes, if none, find with most deleted. That is probably
-- the file with the relevant changes, and so we generate a link from that. Failing that, we just make a link to the
-- front page.
generateLink:: Config -> LogEntry -> URI
generateLink cfg entry = generateURL cfg (view filechanges entry)

-- TODO: Make front page link configurable.
generateURL:: Config -> Maybe FileChanges -> URI
generateURL cfg = substitute_link cfg . find_most_edited cfg
  where find_most_edited cfg (Just changes) = let (name, _, _) = foldl maximum_changed_file ("", 0, 0) changes
                                       in name
        find_most_edited cfg Nothing = ""
        maximum_changed_file orig@(max_fname, max_added, max_deleted) change =
          if (view added change) > max_added then
            (view filename change, view added change, view deleted change)
          else if max_added == 0 then
                 if (view deleted change) > max_deleted then
                   (view filename change, view added change, view deleted change)
                 else
                   orig
               else
                 orig
        -- Replace source extension with target extension, delete source path, prepend destination URL,
        -- parse into URI
        substitute_link config =
          fromJust .
          parseURI .
          ((view fileDestURL cfg) ++) .
          (replace (view fileSourcePath config) "") .
          (replace (view fromExtension config) (view toExtension config))

processEntry:: Config -> LogEntry -> Item
processEntry config entry = [ Title (view title entry),
                              Description (view description entry),
                              Author (view author entry),
                              PubDate (view date entry),
                              Link (generateLink config entry)
                            ]

runMain:: String -> RSSgenErrorIO ()
runMain confpath = do conf <- withExceptT (\e -> Left e) $ readConfig confpath
                      lines <- liftIO getContents
                      entries <- withExceptT (\e -> Right e) $ ExceptT $ return $ parseLog lines "standard input"
                      result <- return $ generateRSS conf (filterAndRemoveRSSToken entries)
                      liftIO $ print $ showXML $ rssToXML result
                      return ()
                      
main = do result <- runExceptT $ runMain "rssgen.conf"
          case result of
            Left err -> print err >> exitFailure
            Right _ -> exitSuccess
            
