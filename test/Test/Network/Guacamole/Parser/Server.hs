module Test.Network.Guacamole.Parser.Server where

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

import           Network.Guacamole.Types.Server (GuacamoleServerRequest (..))

import           Test.Network.Guacamole.Parser.Common

unit_parseSync :: Assertion
unit_parseSync = do
    serverAssert "4.sync,10.1234567890;" $ Sync 1234567890

unit_unparseSync :: Assertion
unit_unparseSync = do
    unparseServerRequest (Sync 1234567890) @?= Right sync
    where
        sync = mkGuacamolePacket "sync" ["1234567890"]
