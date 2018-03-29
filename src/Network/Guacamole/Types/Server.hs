{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Network.Guacamole.Types.Server (
                                      GuacamoleServerRequest(..)
    ) where

import           Universum

import           Data.ByteString (ByteString)

import           Network.Guacamole.Types.Core (GuacamoleRGBA (..), GuacamoleSize (..),
                                               GuacamoleXY (..), TransformLayer (..))
import           Network.Guacamole.Types.Enums (GuacamoleCapStyle (..), GuacamoleCompositeMode (..),
                                                GuacamoleJoinStyle (..), GuacamoleTransfer (..))
import           Network.Guacamole.Types.Pipes (PipeBlob (..), PipeObject (..), PipeRequest (..),
                                                PipeRequestIndexed (..), PipeRequestNamed (..),
                                                PipeStatus (..))

import qualified Generics.SOP as SOP


data GuacamoleServerRequest =
{-    Ack
      { gsrAck :: !PipeStatus
      }
  | Args
      { gsrArgs :: ![ByteString]
      }
  | Arc
      { gsrcLayer     :: !Int
      , gsrXY         :: !GuacamoleXY
      , gsrRadius     :: !Int
      , gsrStartAngle :: !Double
      , gsrEndAngle   :: !Double
      , gsrNegative   :: !Bool
      }
  | Blob
      { gsrBlob :: !PipeBlob
      }
  | Body
      { gsrBody   :: !PipeObject
      }
  | Cfill
      { gsrMode  :: !GuacamoleCompositeMode
      , gsrLayer :: !Int
      }
  | Clip
      { gsrLayer :: !Int
      }
  | Clipboard
      { gsrStream :: !Int
      , gsrMime   :: !ByteString
      }
  | Copy
      { gsrSourceLayer :: !Int
      , gsrSourceXY    :: !GuacamoleXY
      , gsrRectSize    :: !GuacamoleSize
      , gsrW           :: !Int
      , gsrMode        :: !GuacamoleCompositeMode
      , gsrDestLayer   :: !Int
      , gsrDestXY      :: !GuacamoleXY
      }
  | Cstroke
      { gsrMode      :: !GuacamoleCompositeMode
      , gsrLayer     :: !Int
      , gsrCap       :: !GuacamoleCapStyle
      , gsrJoin      :: !GuacamoleJoinStyle
      , gsrThinkness :: !Int
      , gsrRGBA      :: !GuacamoleRGBA
      }
  | Curve
      { grsIndex    :: !Int
      , gsrCurveCp1 :: !GuacamoleXY
      , gsrCurveCp2 :: !GuacamoleXY
      , csrCurveCp3 :: !GuacamoleXY
      }
  | Disconnect
  | Dispose
      { gsrDisposeIndex     :: !Int
      }
  | Distort
      { gsrDistort          :: !TransformLayer
      }
  | End
      { gsrStreamIndex      :: !Int
      }
  | Log
      { gsrLogMessage       :: !ByteString
      }
  | File
      { gsrStreamIndex :: !Int
      , gsrMime        :: !ByteString
      , gsrFileName    :: !ByteString
      }
  | Filesystem -- should be in Pipes?
      { gsrFilesystemIndex :: !Int
      , gsrFilesystemName  :: !ByteString
      }
  | Identity
      { gsrIdentityIndex    :: !Int
      }
  | Lfill
      { gsrLayerIndex  :: !Int
      , gsrSourceIndex :: !Int
      }
  | Line
      { gsrLine             :: !GuacamoleXY
      }
  | Lstroke
      { gsrMode      :: !GuacamoleCompositeMode
      , gsrIndex     :: !Int
      , gsrCap       :: !GuacamoleCapStyle
      , gsrJoin      :: !GuacamoleJoinStyle
      , gsrThinkness :: !Int
      , gsrSource    :: !Int
      }
  | Mouse
      { gsrMouse      :: !GuacamoleXY
      , gsrButtonMask :: !Int
      , gsrTimestamp  :: !Int
      }
  | Move
      { gsrIndex  :: !Int
      , gsrParent :: !Int
      , gsrTo     :: !Int
      , gsrZ      :: !Int
      }
  | Name
      { gsrName             :: !ByteString
      }
  | Nest
      { gsrNest             :: !ByteString
      }
  | Nop
  | Pipe
      { gsrPipe :: !PipeRequestNamed
      }
  | Img
      { gsrStream :: !Int
      , gsrMode   :: !GuacamoleCompositeMode
      , gsrLayer  :: !Int
      , gsrMime   :: !ByteString
      , gsrXY     :: !GuacamoleXY
      }
  | Pop
      { gsrIndex            :: !Int
      }
  | Push
      { gsrIndex            :: !Int
      }
  | Ready
      { gsrSessionId        :: !Int
      }
  | Reset
      { gsrLayer            :: !Int
      }
  | Rect
      { gsrLayer :: !Int
      , gsrXY    :: !GuacamoleXY
      , gsrSize  :: !GuacamoleSize
      }
  | Select
      { gsrProtocol         :: !ByteString
      }
  | Set
      { gsrLayer :: !Int
      , gsrKey   :: !ByteString
      , gsrValue :: !ByteString
      }
  | Shade
      { gsrLayer :: !Int
      , gsrA     :: !Int
      }
  | Size
      { gsrSize             :: !GuacamoleSize
      }
  | Start
      { gsrLayer :: !Int
      , gsrXY    :: !GuacamoleXY
      }
  | -} Sync
      { gsrTimestamp        :: !Int
      } {-
  | Transfer
      { gsrSrcLayer  :: !Int
      , gsrSrc       :: !GuacamoleXY
      , gsrSize      :: !GuacamoleSize
      , gsrTransfer  :: !GuacamoleTransfer
      , gsrDestLayer :: !Int
      , gsrDest      :: !GuacamoleXY
      }
  | Transform
      { gsrTransform        :: !TransformLayer
      }
  | Undefine
      { gsrObject           :: !Int
      }
  | Video
      { gsrVideo :: !PipeRequestIndexed
      }
      -}
  deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)
