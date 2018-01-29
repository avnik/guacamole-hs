module Test.Network.Guacamole.Parser where

import           Data.ByteString
import qualified Data.ByteString.Char8    as BSC8
import           Test.HUnit               (Assertion, (@?=))

import           Network.Guacamole.Packet

unit_parsePacket :: Assertion
unit_parsePacket =
    parseGuacamolePacket examplePacket @?= Right expected
    where
        expected = mkGuacamolePacket "a" ["bc", "def", "helloworld"]

examplePacket = BSC8.pack "1.a,2.bc,3.def,10.helloworld;"
-- examplePacket = "1.a,2.bc,3.def,10.helloworld;4.test,5.test2;0.;3.foo;" :: ByteString
