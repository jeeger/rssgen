{-# LANGUAGE  TemplateHaskell #-}
module LogParser (LogEntry, LogEntries, FileChange, parseLog, filename, added, deleted, title, description, author, date, guid, filechanges) where

import Data.Time
import Control.Lens hiding (noneOf)
import Data.Time.Format
import Data.Time.Clock
import Control.Applicative hiding (optional,(<|>),many)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


-- Filename, Added lines, deleted lines
data FileChange = FileChange { _filename :: String,
                               -- No added or deleted lines for binary files.
                               _added :: Maybe Integer,
                               _deleted :: Maybe Integer
                             } deriving (Show)
                                        
type FileChanges = [FileChange]

data LogEntry = LogEntry { _title :: String,
                           _description :: String,
                           _author :: String,
                           _date :: Data.Time.UTCTime,
                           _guid :: String,
                           -- Commits without file changes are possible (git commit --allow-empty)
                           _filechanges :: Maybe FileChanges
                   } deriving (Show)


$(makeLenses ''LogEntry)
$(makeLenses ''FileChange)

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
                       return FileChange {_filename = filename,
                                          _added = added,
                                          _deleted = deleted}

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
  return LogEntry { _guid = commitid,
                    _author = author,
                    _title = title,
                    _description = description,
                    _date = (parseTimeOrError True defaultTimeLocale rfc822DateFormat datestring):: UTCTime,
                    _filechanges = changes
               }

logparser:: Parser LogEntries
logparser = do entries <- many1 commit
               eof
               return entries

parseLog input inputname = parse logparser inputname input
