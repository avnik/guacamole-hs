module Test.Network.Guacamole.Parser.Client where

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
import           Network.Guacamole.Types.Client (GuacamoleClientRequest (..))
import           Network.Guacamole.Types.Core (GuacamoleSize (..), GuacamoleXY (..))
import           Network.Guacamole.Types.Enums (GuacamoleStatusEnum (..))
import           Network.Guacamole.Types.Pipes (PipeBlob (..), PipeRequestNamed (..),
                                                PipeStatus (..))
import           Test.Network.Guacamole.Parser.Common

{-----------------------------------------------------------------------------------------------
   -- Stuff taken from live protocol (native C/JS/python implementations)
 -----------------------------------------------------------------------------------------------}

unit_parseSync :: Assertion
unit_parseSync = do
    clientAssert "4.sync,10.1234567890;" $ Sync 1234567890

unit_unparseSync :: Assertion
unit_unparseSync = do
    unparseClientRequest (Sync 1234567890) @?= Right sync
    where
        sync = mkGuacamolePacket "sync" ["1234567890"]

unit_parseKey :: Assertion
unit_parseKey = do
    clientAssert "3.key,2.42,2.24;" $ Key 42 24

unit_parseMouse :: Assertion
unit_parseMouse = do
    clientAssert "5.mouse,2.42,2.24,3.420;" $ Mouse (GuacamoleXY 42 24) 420

unit_parseSize :: Assertion
unit_parseSize = do
    clientAssert "4.size,4.1024,3.768;" $ Size $ GuacamoleSize 1024 768

unit_parseBlob :: Assertion
unit_parseBlob = do
    clientAssert "4.blob,2.42,14.blob-blob-blob;" $ Blob (PipeBlob 42 "blob-blob-blob")

unit_parsePipe :: Assertion
unit_parsePipe = do
    clientAssert "4.pipe,2.42,7.control,16.application/json;" $ Pipe $ PipeRequestNamed 42 "control" "application/json"

unit_parseFile :: Assertion
unit_parseFile = do
    clientAssert "4.pipe,2.42,12.control.json,16.application/json;" $ Pipe $ PipeRequestNamed 42 "control.json" "application/json"

unit_parseEnd :: Assertion
unit_parseEnd = do
    clientAssert "3.end,2.42;" $ End 42

unit_parseAck :: Assertion
unit_parseAck = do
    clientAssert "3.ack,2.42,6.All ok,1.0;"  $ Ack $ PipeStatus 42 "All ok" StatusSuccess

unit_parseDisconnect :: Assertion
unit_parseDisconnect = do
    clientAssert "10.disconnect;" $ Disconnect

unit_parsePut :: Assertion
unit_parsePut = do
    clientAssert "3.put,2.42,3.420,16.application/json,12.control.json;" $ Put 42 420 "application/json" "control.json"

unit_parseGet :: Assertion
unit_parseGet = do
    clientAssert "3.get,2.42,12.control.json;" $ Get 42 "control.json"
