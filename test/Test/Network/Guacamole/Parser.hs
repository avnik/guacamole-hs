module Test.Network.Guacamole.Parser where

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

unit_encodingTest :: Assertion
unit_encodingTest = do
    encodeActions (foo <> bar) @?= Right foobar
        where
            foobar = mkGuacamolePacket "foo" ["bar"]
            foo = toEncoding ("foo" :: Text)
            bar = toEncoding ("bar" :: Text)

packetAssert :: String -> GuacamolePacket -> Assertion
packetAssert sPacket packet = do
    parseGuacamolePacket bsPacket @?= Right packet
    unparseGuacamolePacketStrict packet @?= Right bsPacket
    where
        bsPacket = BSC8.pack sPacket

unit_parseBadClientOpcode :: Assertion
unit_parseBadClientOpcode = do
    (parseGuacamolePacket cr >>= parseClientRequest)  @?= Left "Unknown client command: xyz"
    where
        cr = BSC8.pack "3.xyz;"

unit_parsePacket :: Assertion
unit_parsePacket = do
    packetAssert "1.a,2.bc,3.def,10.helloworld;" $ mkGuacamolePacket "a" ["bc", "def", "helloworld"]
    packetAssert "6.single;" $ mkGuacamolePacket "single" []
    packetAssert "3.one,3.two;" $ mkGuacamolePacket "one" ["two"]
