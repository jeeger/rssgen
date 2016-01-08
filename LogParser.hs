module LogParser where

import Data.Time
import Data.Time.Format
import Data.Time.Clock
import Control.Applicative hiding (optional,(<|>),many)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


-- Filename, Added lines, deleted lines
data FileChange = FileChange { filename :: String,
                               -- No added or deleted lines for binary files.
                               added :: Maybe Integer,
                               deleted :: Maybe Integer
                             } deriving (Show)
                                        
type FileChanges = [FileChange]

data LogEntry = LogEntry { title :: String,
                     description :: String,
                     author :: String,
                     date :: Data.Time.UTCTime,
                     guid :: String,
                     -- Commits without file changes are possible (git commit --allow-empty)
                     filechanges :: Maybe FileChanges
                   } deriving (Show)

type LogEntries = [LogEntry]

-- For the natural number parser.
lexer = Token.makeTokenParser emptyDef
natural = Token.natural lexer

parseField:: Parser String
parseField = many (noneOf "\x1f") <* char '\x1f'

numberOrDash:: Parser (Maybe Integer)
numberOrDash = do num <- try (optionMaybe natural)
                  case num of
                    Just n -> do
                      return $ Just n
                    Nothing -> do
                      char '-'
                      return $ Nothing

parseSingleChange:: Parser FileChange
parseSingleChange = do added <- numberOrDash
                       spaces
                       deleted <- numberOrDash
                       spaces
                       filename <- many1 (noneOf "\x00")
                       char '\x00'
                       return FileChange {filename = filename,
                                          added = added,
                                          deleted = deleted}

parseRestChanges:: Parser [FileChange]
parseRestChanges = ((lookAhead $ char '\x1f') >> return []) <|>
                   ((lookAhead eof) >> return []) <|>
                   parseChangeList

parseChangeList:: Parser FileChanges
parseChangeList = do first <- parseSingleChange
                     rest <- parseRestChanges
                     return $ first : rest

commit:: Parser LogEntry
commit = do
  char '\x1f'
  commitid <- parseField
  author <- parseField
  datestring <- parseField
  title <- parseField
  description <- parseField
  char '\x00'
  changes <- optionMaybe (try (newline *> parseChangeList))
  return LogEntry { guid = commitid,
                 author = author,
                 title = title,
                 description = description,
                 date = (parseTimeOrError True defaultTimeLocale rfc822DateFormat datestring):: UTCTime,
                 filechanges = changes
               }

logparser:: Parser LogEntries
logparser = do entries <- many1 commit
               eof
               return entries

parseLog input inputname = parse logparser inputname input
