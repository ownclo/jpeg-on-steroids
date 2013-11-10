{-# LANGUAGE OverloadedStrings #-}

import Data.Word
import Data.Attoparsec.Char8
import Control.Monad.State
import Control.Applicative

import qualified Data.ByteString.Char8 as BS

data IP = IP Word8 Word8 deriving Show
data TP = TP Word8 deriving Show

-- Our parser will modify the environment.
data Env =
     Env { ip :: !IP
         , tp :: !TP
     } deriving Show

-- say, we have parsers for these
-- data structures:
parseIP :: Parser IP
parseIP = do
        string "IP: "
        [d1, d2] <- decimal `sepBy` dot
        return $ IP d1 d2
            where dot = char '.'

parseTP :: Parser TP
parseTP = do
        string "TP: "
        TP `liftM` decimal

-- Now, we want to update the returned value
-- when a new IP is returned.
type ParseEnv = StateT Env Parser

runEnv :: ParseEnv a -> BS.ByteString -> Either String (a, Env)
runEnv f = parseOnly (runStateT f (error "NO ENV"))

modifyIP :: ParseEnv ()
modifyIP = do
        ip <- lift parseIP
        modify $ \env -> env {ip = ip}

modifyTP = do
        tp <- lift parseTP
        modify $ \env -> env {tp = tp}

-- The order in which the environment is updated shall
-- not be specified strictly.
parseHeader :: ParseEnv ()
-- XXX: That code does not work yet.
parseHeader = many $ ((modifyIP <|> modifyTP) `sepBy` char ' ')

main = case runEnv parseHeader s of
           Right (_, env) -> print env
           Left err       -> print err
       where s = "IP: 10.11 TP: 15"
--        where s = "TP: 11 IP: 13.15"
