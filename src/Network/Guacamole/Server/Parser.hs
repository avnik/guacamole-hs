module Network.Guacamole.Server.Parser (
                                        serverRequestParser
                                       ) where

import           Universum

import           Network.Guacamole.Parser.Internal (Parser (..), chunk, failure, float, integral,
                                                    text)
import           Network.Guacamole.Parser.Prims (parseRGBA, parseSize, parseXY)
import           Network.Guacamole.Server.Types (GuacamoleServerRequest (..))

serverRequestParser :: Text -> Parser GuacamoleServerRequest
serverRequestParser = \case
    "sync" -> Sync <$> integral
    unknown -> failure $ "Unknown server command: " <> unknown
