module Test.Network.Guacamole.Parser where

import           Universum

import           Data.ByteString
import qualified Data.ByteString.Char8          as BSC8
import qualified Data.ByteString.Lazy.Char8     as BSL8
import           Test.HUnit                     (Assertion, (@?=))

import           Network.Guacamole.Encoder      (unparseClientRequest,
                                                 unparseServerRequest)
import           Network.Guacamole.Packet       (GuacamolePacket (..),
                                                 mkGuacamolePacket,
                                                 parseGuacamolePacket,
                                                 unparseGuacamolePacketStrict)
import           Network.Guacamole.Parser       (parseClientRequest,
                                                 parseServerRequest)
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


unit_parseSync :: Assertion
unit_parseSync = do
    clientAssert "4.sync,10.1234567890;" $ C.Sync 1234567890
    serverAssert "4.sync,10.1234567890;" $ S.Sync 1234567890


unit_parsePacket :: Assertion
unit_parsePacket = do
    parseGuacamolePacket bsPacket @?= Right packet
    unparseGuacamolePacketStrict packet @?= Right bsPacket
    where
        packet :: GuacamolePacket
        packet = mkGuacamolePacket "a" ["bc", "def", "helloworld"]

        bsPacket :: ByteString
        bsPacket = BSC8.pack "1.a,2.bc,3.def,10.helloworld;"
