module Network.Guacamole.Parser.Prims (
                                        parseXY
                                      , parseSize
                                      , parseRGBA
                                      , parseTransformLayer
    ) where

import           Universum

import           Network.Guacamole.Parser.Internal (Parser (..), chunk, float, integral)
import           Network.Guacamole.Server.Types (TransformLayer (..))
import           Network.Guacamole.Types (GuacamoleEnumEncoding (..), GuacamoleRGBA (..),
                                          GuacamoleSize (..), GuacamoleXY (..))

parseXY :: Parser GuacamoleXY
parseXY = GuacamoleXY <$> integral <*> integral

parseSize :: Parser GuacamoleSize
parseSize = GuacamoleSize <$> integral <*> integral

parseRGBA :: Parser GuacamoleRGBA
parseRGBA = GuacamoleRGBA <$> integral <*> integral <*> integral <*> integral

parseEnum :: GuacamoleEnumEncoding a => Parser a
parseEnum = toGuacamoleEnum <$> integral

parseTransformLayer :: Parser TransformLayer
parseTransformLayer = TransformLayer <$> integral
                                     <*> float <*> float
                                     <*> float <*> float
                                     <*> float <*> float
