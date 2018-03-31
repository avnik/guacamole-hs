module Test.Network.Guacamole.Parser.Common where

import           Universum

import           Data.ByteString
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Test.HUnit (Assertion, (@?=))

import           Network.Guacamole.Encoder (EncodingAction (..), GuacamoleEncoder (..),
                                            encodeActions, unparseClientRequest,
                                            unparseServerRequest)
import           Network.Guacamole.Packet (GuacamolePacket (..), mkGuacamolePacket,
                                           parseGuacamolePacket, unparseGuacamolePacketStrict)
import           Network.Guacamole.Parser (parseClientRequest, parseServerRequest)
import           Network.Guacamole.Types.Core (GuacamoleXY (..))

import qualified Network.Guacamole.Types.Client as C
import qualified Network.Guacamole.Types.Server as S

serverAssert :: String -> S.GuacamoleServerRequest -> Assertion
serverAssert s gsr = do
    parsed @?= Right gsr
    unparsed @?= Right bs
    where
        parsed = parseGuacamolePacket bs >>= parseServerRequest
        unparsed = unparseServerRequest gsr >>= unparseGuacamolePacketStrict
        bs = BSC8.pack s

clientAssert :: String -> C.GuacamoleClientRequest -> Assertion
clientAssert s gcr = do
    parsed @?= Right gcr
    unparsed @?= Right bs
    where
        parsed = parseGuacamolePacket bs >>= parseClientRequest
        unparsed = unparseClientRequest gcr >>= unparseGuacamolePacketStrict
        bs = BSC8.pack s
