{-# LANGUAGE LambdaCase #-}
module Network.Guacamole.Parser.Client (
                                        clientRequestParser
                                       ) where

import           Universum

import           Network.Guacamole.Parser.Internal (Parser (..), chunk, failure, float, integral,
                                                    text)
import           Network.Guacamole.Parser.Pipes (parsePipeBlob, parsePipeRequest,
                                                 parsePipeRequestIndexed, parsePipeRequestNamed,
                                                 parsePipeStatus)
import           Network.Guacamole.Parser.Prims (parseRGBA, parseSize, parseXY)
import           Network.Guacamole.Types.Client (GuacamoleClientRequest (..))

clientRequestParser :: Text -> Parser GuacamoleClientRequest
clientRequestParser = \case
    "sync" -> Sync <$> integral
    "key" -> Key <$> integral <*> integral
    "mouse" -> Mouse <$> parseXY <*> integral
    "size" -> Size <$> parseSize
    "clipboard" -> Clipboard <$> parsePipeRequest
    "blob" -> Blob <$> parsePipeBlob
    "ack" -> Ack <$> parsePipeStatus
    "pipe" -> Pipe <$> parsePipeRequestNamed
    "file" -> Pipe <$> parsePipeRequestNamed
    "audio" -> Audio <$> parsePipeRequestIndexed
    "end" -> End <$> integral
    "get" -> Get <$> integral <*> chunk
    "put" -> Put <$> integral <*> integral <*> chunk <*> chunk
    "disconnect" -> pure Disconnect

    unknown -> failure $ "Unknown client command: " <> unknown
