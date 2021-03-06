module Network.Guacamole.Parser (
    parseClientRequest
  , parseServerRequest
) where

import           Universum

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Network.Guacamole.Packet (GuacamolePacket (..))
import           Network.Guacamole.Parser.Client (clientRequestParser)
import           Network.Guacamole.Parser.Internal (Parser (..), runParser)
import           Network.Guacamole.Server.Parser (serverRequestParser)
import           Network.Guacamole.Types.Client (GuacamoleClientRequest)
import           Network.Guacamole.Types.Server (GuacamoleServerRequest)


parseClientRequest :: GuacamolePacket -> Either Text GuacamoleClientRequest
parseClientRequest = parseWith clientRequestParser

parseServerRequest :: GuacamolePacket -> Either Text GuacamoleServerRequest
parseServerRequest = parseWith serverRequestParser

parseWith :: (Text -> Parser a) -> GuacamolePacket -> Either Text a
parseWith getParser packet =
    runParser (getParser $ T.decodeLatin1 $ gpKeyword packet) (gpParams packet)

