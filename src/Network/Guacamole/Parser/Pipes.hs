{-# LANGUAGE LambdaCase #-}
module Network.Guacamole.Parser.Pipes (
                                         parsePipeRequest
                                       , parsePipeRequestNamed
                                       , parsePipeRequestIndexed
                                       , parsePipeStatus
                                       , parsePipeBlob
                                       ) where

import           Universum

import           Network.Guacamole.Parser.Internal (Parser (..), chunk, failure, float, integral,
                                                    text)
import           Network.Guacamole.Parser.Prims (parseEnum)
import           Network.Guacamole.Types.Pipes (PipeBlob (..), PipeObject (..), PipeRequest (..),
                                                PipeRequestIndexed (..), PipeRequestNamed (..),
                                                PipeStatus (..))

-- Handwritten crappy-parser, until we have derived one works

parsePipeStatus :: Parser PipeStatus
parsePipeStatus =
    PipeStatus <$> integral <*> chunk <*> parseEnum

parsePipeRequest :: Parser PipeRequest
parsePipeRequest =
    PipeRequest <$> integral <*> chunk

parsePipeRequestNamed :: Parser PipeRequestNamed
parsePipeRequestNamed =
    PipeRequestNamed <$> integral <*> chunk <*> chunk

parsePipeRequestIndexed :: Parser PipeRequestIndexed
parsePipeRequestIndexed =
    PipeRequestIndexed <$> integral <*> integral <*> chunk

parsePipeBlob :: Parser PipeBlob
parsePipeBlob =
    PipeBlob <$> integral <*> chunk
