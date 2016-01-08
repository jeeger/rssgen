module LogParser where

import Data.Time
import Data.Time.Format
import Data.Time.Clock
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data LogEntry = LogEntry { title :: String,
                     description :: String,
                     author :: String,
                     date :: Data.Time.UTCTime,
                     guid :: String,
                     rssworthy :: Bool
                   } deriving (Show)

type LogEntries = [LogEntry]

parseField:: Parser String
parseField = manyTill anyChar ((char '\x1f') <|> (char '\x1e')) >>= return

commit:: Parser LogEntry
commit = do
  commitid <- parseField
  author <- parseField
  datestring <- parseField
  title <- parseField
  -- Optional description, goes to EOF or next "\ncommit" line.
  description <- parseField
  return LogEntry { guid = commitid,
                 author = author,
                 title = title,
                 description = description,
                 date = (parseTimeOrError True defaultTimeLocale rfc822DateFormat datestring):: UTCTime,
                 rssworthy = False
               }

logparser:: Parser LogEntries
logparser = do entries <- many1 commit
               eof
               return entries

parseLog input inputname = parse logparser inputname input
