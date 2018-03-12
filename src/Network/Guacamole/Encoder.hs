module Network.Guacamole.Encoder (
                                   GuacamoleEncoder(..)
                                 , encodePacket
                                 , encodeChunk
    ) where

import           Universum

import qualified Data.ByteString                as BS8
import qualified Data.ByteString.Builder        as BE
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T

import           Network.Guacamole.Server.Types (TransformLayer (..))
import           Network.Guacamole.Types        (GuacamoleEnumEncoding,
                                                 GuacamoleRGBA (..),
                                                 GuacamoleSize (..),
                                                 GuacamoleXY (..))


class GuacamoleEncoder a where
    toEncoding :: a -> BE.Builder

instance GuacamoleEncoder Int where
    toEncoding  = encodeChunk . BE.intDec

instance GuacamoleEncoder Integer where
    toEncoding = encodeChunk . BE.integerDec

instance GuacamoleEncoder Float where
    toEncoding = encodeChunk . BE.floatDec

instance GuacamoleEncoder Double where
    toEncoding = encodeChunk . BE.doubleDec

instance GuacamoleEncoder GuacamoleXY where
    toEncoding (GuacamoleXY x y) = toEncoding x <> toEncoding y

instance GuacamoleEncoder GuacamoleSize where
    toEncoding (GuacamoleSize h w) = toEncoding h <> toEncoding w

instance GuacamoleEncoder TransformLayer where
    toEncoding (TransformLayer layer a b c d e f) = toEncoding layer <> toEncoding a <> toEncoding b <> toEncoding c <> toEncoding d <> toEncoding e <> toEncoding f

instance GuacamoleEncoder GuacamoleRGBA where
    toEncoding (GuacamoleRGBA r g b a) = toEncoding r <> toEncoding g <> toEncoding b <> toEncoding a

instance GuacamoleEncoder ByteString where
    toEncoding = encodeChunk

instance GuacamoleEncoder Text where
    toEncoding = encodeChunk . T.encodeUtf8

instance GuacamoleEncoder (GuacamoleEnumEncoding a) where
    toEncoding = toEncoding . fromGuacamoleEnum

encodePacket :: Text -> BE.Builder -> BE.Builder
encodePacket keyword args =
    encodeChunk . T.encodeUtf8 keyword <> args

encodeChunk :: BE.Builder -> BE.Builder
encodeChunk chunk =
    BE.intDec . BS8.length chunk <> char7 '.' <> BE.byteString . toByteString chunk
