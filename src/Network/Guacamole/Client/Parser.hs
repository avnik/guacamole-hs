module Network.Guacamole.Client.Parser (
                                        clientRequestParser
                                       ) where

import           Universum

import           Network.Guacamole.Client.Types (GuacamoleClientRequest (..))
import           Network.Guacamole.Parser.Internal (Parser (..), chunk, failure, float, integral,
                                                    text)
import           Network.Guacamole.Parser.Prims (parseRGBA, parseSize, parseXY)

clientRequestParser :: Text -> Parser GuacamoleClientRequest
clientRequestParser = \case
    "sync" -> Sync <$> integral
    unknown -> failure $ "Unknown client command: " <> unknown
