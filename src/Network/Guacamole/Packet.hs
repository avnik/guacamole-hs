module Network.Guacamole.Packet (
                                  GuacamolePacket(..)
                                , buildGuacamolePacket
                                , parseGuacamolePacket
                                , mkGuacamolePacket
    )
    where

import           Data.Attoparsec.ByteString       (Parser (..), parseOnly)
import           Data.Attoparsec.ByteString.Char8 (char, decimal)
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.Attoparsec.Combinator       (many')
import           Data.ByteString                  (ByteString (..))
import qualified Data.ByteString                  as BS8
import           Data.ByteString.Builder          (Builder, byteString, char7,
                                                   intDec, toLazyByteString)
import qualified Data.ByteString.Char8            as BSC8
import qualified Data.ByteString.Lazy             as LBS8
import           Data.Foldable                    (foldMap)
import           Data.Monoid

data GuacamolePacket = GuacamolePacket
    { gpKeyword :: ByteString
    , gpParams  :: [ByteString]
    } deriving (Eq, Ord, Show)

mkGuacamolePacket :: String -> [String] -> GuacamolePacket
mkGuacamolePacket kw params = GuacamolePacket (BSC8.pack kw) (map BSC8.pack params)

buildGuacamolePacket :: GuacamolePacket -> LBS8.ByteString
buildGuacamolePacket = toLazyByteString . buildPacket

buildPacket :: GuacamolePacket -> Builder
buildPacket gp = lenPrefixed (gpKeyword gp) <> foldMap lenPrefixed (gpParams gp)
    where
        lenPrefixed :: ByteString -> Builder
        lenPrefixed t = (lenP t) <> char7 '.' <> byteString t

        lenP :: ByteString -> Builder
        lenP = intDec . BS8.length

parseGuacamolePacket :: ByteString -> Either String GuacamolePacket
parseGuacamolePacket = parseOnly guacamoleParser

guacamoleParser :: Parser GuacamolePacket
guacamoleParser = GuacamolePacket <$> prefixedBody <*> many' (char ',' *> prefixedBody) <* char ';'

prefixedBody :: Parser ByteString
prefixedBody = do
    len <- decimal
    _ <- char '.'
    body <- A.take len
    return body
