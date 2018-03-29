{-# LANGUAGE CPP #-}

module Network.Guacamole.Packet (
                                  GuacamolePacket(..)
                                , unparseGuacamolePacket
                                , unparseGuacamolePacketStrict
                                , parseGuacamolePacket
                                , mkGuacamolePacket
                                , encodeLenPrefixedBody
    )
    where

import           Universum

import           Data.Attoparsec.ByteString (Parser (..), parseOnly)
import           Data.Attoparsec.ByteString.Char8 (char, decimal)
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.Attoparsec.Combinator (many')
import           Data.ByteString (ByteString (..))
import qualified Data.ByteString as BS8
import           Data.ByteString.Builder (Builder, byteString, char7, intDec, toLazyByteString)
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as LBS8
import qualified Data.Text as T


-- From guacamole/parser-constants.h
#define GUAC_INSTRUCTION_MAX_LENGTH 8192
#define GUAC_INSTRUCTION_MAX_DIGITS 5
#define GUAC_INSTRUCTION_MAX_ELEMENTS 64


data GuacamolePacket = GuacamolePacket
    { gpKeyword :: ByteString
    , gpParams  :: [ByteString]
    } deriving (Eq, Ord, Show)

mkGuacamolePacket :: String -> [String] -> GuacamolePacket
mkGuacamolePacket kw params = GuacamolePacket (BSC8.pack kw) (map BSC8.pack params)

unparseGuacamolePacket :: GuacamolePacket -> Either Text LBS8.ByteString
unparseGuacamolePacket = Right . toLazyByteString . buildPacket

unparseGuacamolePacketStrict :: GuacamolePacket -> Either Text ByteString
unparseGuacamolePacketStrict = Right . LBS8.toStrict . toLazyByteString . buildPacket

buildPacket :: GuacamolePacket -> Builder
buildPacket gp = encodeLenPrefixedBody (gpKeyword gp) <> delim <>  mconcat arguments <> char7 ';'
    where
        arguments = intersperse delim $ encodeLenPrefixedBody <$> gpParams gp
        delim = char7 ','

encodeLenPrefixedBody :: ByteString -> Builder
encodeLenPrefixedBody t = (lenP t) <> char7 '.' <> byteString t
    where
        lenP :: ByteString -> Builder
        lenP = intDec . BS8.length
{-# INLINE encodeLenPrefixedBody #-}

parseGuacamolePacket :: ByteString -> Either Text GuacamolePacket
parseGuacamolePacket bs =  case  parseOnly guacamoleParser bs of
                             Left e  -> Left $ T.pack e
                             Right r -> Right r


guacamoleParser :: Parser GuacamolePacket
guacamoleParser = GuacamolePacket <$> prefixedBody <*> many' (char ',' *> prefixedBody) <* char ';'

prefixedBody :: Parser ByteString
prefixedBody = do
    len <- decimal
    _ <- char '.'
    body <- A.take len
    return body
