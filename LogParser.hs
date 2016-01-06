module LogParser where

import Data.Time
import Control.Monad
import Data.Time.Format
import Data.Time.Clock
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Entry = Entry { url :: String,
                     title :: String,
                     description :: String,
                     author :: String,
                     date :: Data.Time.UTCTime,
                     guid :: String,
                     rssworthy :: Bool
                   } deriving (Show)

type Entries = [Entry]
                                        
dateKey = "Date:"
authorKey = "Author:"
commitKey = "commit"

languageDef = emptyDef {
  Token.reservedOpNames = [ dateKey, authorKey,  commitKey ],
  -- We abuse identifiers for commit IDs.
  Token.identStart = alphaNum,
  Token.identLetter = alphaNum
  }

lexer = Token.makeTokenParser languageDef
reservedOp = Token.reservedOp lexer
whiteSpace = Token.whiteSpace lexer
identifier = Token.identifier lexer
symbol = Token.symbol lexer

commitLine:: Parser String
commitLine = do char '\n'
                reservedOp commitKey
                commitId <- identifier
                return commitId
                        
commit:: Parser Entry
commit = do commitid <- commitLine
            -- Line only present for merges.
            optional $ do
              string "Merge:"
              whiteSpace
              manyTill anyChar (try (char '\n'))
            reservedOp authorKey
            author <- manyTill anyChar  (try (string " <"))
            -- Not used currently
            mail <- manyTill anyChar (try (char '>'))
            whiteSpace
            reservedOp dateKey
            datestring <- manyTill anyChar (try (char '\n'))
            -- Now the proper commit message. First, newline + spaces.
            whiteSpace
            -- Now, Title. One line (according to proper commit message format)
            title <- manyTill anyChar (try (char '\n'))
            -- Optional description
            description <- manyTill anyChar (try (lookAhead commitLine))
            return Entry { guid = commitid,
                           author = author,
                           title = title,
                           description = description,
                           url = "Test",
                           date = (Data.Time.Format.parseTimeOrError True defaultTimeLocale rfc822DateFormat datestring):: UTCTime,
                           rssworthy = False
                           }

logparser:: Parser Entries
logparser = many1 commit

parseLog input inputname = parse logparser inputname input
