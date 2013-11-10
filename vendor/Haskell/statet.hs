{-# LANGUAGE OverloadedStrings #-}

import Data.Word
import Data.Attoparsec.Char8
import Control.Monad.State
import Control.Applicative

import qualified Data.ByteString.Char8 as BS

data IP = IP Word8 Word8 deriving Show
data TP = TP Word8 deriving Show

-- Our parser will modify the environment.
-- NOTE: Fields are not strict, because they are
-- initialized as 'error', so we do not want them
-- to be evaluated.
data Env =
     Env { ip :: IP
         , tp :: TP
     } deriving Show

-- say, we have parsers for these
-- data structures:
parseIP :: Parser IP
parseIP = do
        string "IP:"
        [d1, d2] <- decimal `sepBy` dot
        return $ IP d1 d2
            where dot = char '.'

parseTP :: Parser TP
parseTP = do
        string "TP:"
        TP `liftM` decimal

-- Now, we want to dynamically update the returned value
-- when a structure appears in the bytestream. For that,
-- we define a state modifier ParseEnv whith the type
--      ParseEnv a :: Env -> Parser (a, Env)
-- so that the environment is modified when a parser succeeds.
type ParseEnv = StateT Env Parser

runEnv :: ParseEnv a -> BS.ByteString -> Either String (a, Env)
runEnv f = parseOnly $
           runStateT f Env {ip = error "No IP provided",
                            tp = error "No TP provided"}

-- Afterwards, we lift the primitive parser
-- up to ParseEnv and modify the state of
-- environment with respect to results of parsing.
modifyIP :: ParseEnv ()
modifyIP = do
        ip <- lift parseIP
        modify $ \env -> env {ip = ip}

modifyTP :: ParseEnv ()
modifyTP = do
        tp <- lift parseTP
        modify $ \env -> env {tp = tp}

-- The order in which the environment is updated shall
-- not be specified strictly, so we use the alternative
-- combinator. The tricky part here is that we can not
-- use AttoParsec's combinators here.
parseHeader :: ParseEnv ()
parseHeader = void . many $ modifyIP <|> modifyTP

main = case runEnv parseHeader s of
           Right (_, env) -> print env
           Left err       -> print err
--        where s = "IP:10.11" -- error: No TP provided.
--        where s = "IP:10.11TP:15"
       where s = "TP:11IP:13.15IP:14.18" -- the IP is overriden.
