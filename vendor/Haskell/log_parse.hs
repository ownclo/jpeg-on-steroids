{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative

import Data.Attoparsec.Char8
import Data.Word
import Data.Time hiding (parseTime)
import qualified Data.ByteString.Char8 as BS

data IP = IP Word8 Word8 Word8 Word8 deriving Show
data Product = Mouse | Keyboard | Monitor | Speakers deriving Show
data Source = Internet | Friend | NoAnswer deriving Show

data LogEntry =
    LogEntry {
        entryTime    :: LocalTime,
        entryIP      :: IP,
        entryProduct :: Product,
        source       :: Source
    } deriving Show

type Log = [LogEntry]

--- Sources and products textual representations
-- could be done with template haskell.
sources :: [(BS.ByteString, Source)]
sources =  [("internet", Internet)
           ,("friend", Friend)
           ,("noanswer", NoAnswer)]

products :: [(BS.ByteString, Product)]
products = [("mouse", Mouse)
           ,("keyboard", Keyboard)
           ,("monitor", Monitor)
           ,("speakers", Speakers)]

fromString :: (BS.ByteString, a) -> Parser a
fromString (s, a) = string s >> return a

eitherOf :: [(BS.ByteString, a)] -> Parser a
eitherOf = choice . map fromString


parseTime :: Parser LocalTime
parseTime = do
    y  <- 4 `count` digit <* char '-'
    mm <- 2 `count` digit <* char '-'
    d  <- 2 `count` digit <* char ' '
    h  <- 2 `count` digit <* char ':'
    m  <- 2 `count` digit <* char ':'
    s  <- 2 `count` digit
    return $
        LocalTime { localDay = fromGregorian (read y) (read mm) (read d),
                    localTimeOfDay = TimeOfDay (read h) (read m) (read s)
        }

parseIP :: Parser IP
parseIP = do
    [d1, d2, d3, d4] <- decimal `sepBy` dot
    return $ IP d1 d2 d3 d4
        where dot = char '.'

parseProduct :: Parser Product
parseProduct = eitherOf products

parseSource :: Parser Source
parseSource = eitherOf sources

parseLogEntry :: Parser LogEntry
parseLogEntry = do
    t  <- parseTime
    ip <- space *> parseIP
    p  <- space *> parseProduct
    s  <- option NoAnswer $ space *> parseSource
    return $ LogEntry t ip p s

parseLog :: Parser Log
parseLog = many $ parseLogEntry <* endOfLine

main :: IO ()
-- main = print $ parseOnly parseIP "131.45.68.123"
-- main = print $ parseOnly parseTime "2013-06-30 14:33:29"
-- main = print $ parseOnly parseProduct "mouse"
-- main = print $ parseOnly parseSource "internet"

-- XXX: It will fail if no newline present in the end.
main = print $ parseOnly parseLog "2013-06-29 11:16:23 124.67.34.60 keyboard\n"
