{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Network.Guacamole.Encoder (
                                   unparseServerRequest
                                 , unparseClientRequest

                                 -- For testing
                                 , EncodingAction(..)
                                 , encodeActions
                                 , GuacamoleEncoder(..)
    ) where

import           Universum hiding (All, Generic)

import qualified Data.ByteString.Builder as BE
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import           Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Generics.SOP (All, All2, Code, ConstructorInfo (..), ConstructorName,
                               DatatypeInfo (..), DatatypeName, FieldInfo (..), FieldName (..),
                               Generic, HasDatatypeInfo (..), I (..), K (..), NP (..), NS (..),
                               SListI, SOP (..), from, hcliftA, hcliftA2, hcollapse, to, unI, unSOP)


import           Network.Guacamole.Packet (GuacamolePacket (..))
import           Network.Guacamole.Types.Class (GuacamoleEnumEncoding (..))
import           Network.Guacamole.Types.Client (GuacamoleClientRequest)
import           Network.Guacamole.Types.Core (GuacamoleRGBA (..), GuacamoleSize (..),
                                               GuacamoleXY (..), TransformLayer (..))
import           Network.Guacamole.Types.Enums (GuacamoleCapStyle (..), GuacamoleCompositeMode (..),
                                                GuacamoleJoinStyle (..), GuacamoleStatusEnum (..),
                                                GuacamoleTransfer (..))
import           Network.Guacamole.Types.Pipes (PipeBlob (..), PipeObject (..), PipeRequest (..),
                                                PipeRequestIndexed (..), PipeRequestNamed (..),
                                                PipeStatus (..))
import           Network.Guacamole.Types.Server (GuacamoleServerRequest)
import           Network.Guacamole.Types.SOP (EncodingInfo (..), EncodingOptions (..), Tag (..),
                                              defaultEncodingOptions, encodingInfo)

unparseClientRequest :: GuacamoleClientRequest -> Either Text GuacamolePacket
unparseClientRequest = encodeActions . gUnparse

unparseServerRequest :: GuacamoleServerRequest -> Either Text GuacamolePacket
unparseServerRequest = encodeActions . gUnparse

data EncodingAction =
    Plain [BE.Builder]
  | Error Text

instance Monoid EncodingAction where
    mempty = Plain []
    mappend e@(Error _) _       = e
    mappend _ e@(Error _)       = e
    mappend (Plain l) (Plain r) = Plain (l `mappend` r)

instance Semigroup EncodingAction where
    (<>) = mappend

-- Black magic starts here

class GuacamoleEncoder a where
    toEncoding :: a -> EncodingAction
    default toEncoding :: (Generic a, HasDatatypeInfo a, All2 GuacamoleEncoder (Code a)) => a -> EncodingAction
    toEncoding a = gUnparse a

gUnparse :: forall a. (Generic a, All2 GuacamoleEncoder (Code a), HasDatatypeInfo a) => a -> EncodingAction
gUnparse a =
    hcollapse $ hcliftA2 allFP gUnparse' (encodingInfo (Proxy :: Proxy a) defaultEncodingOptions) (unSOP $ from a )
  where
      gUnparse' :: All GuacamoleEncoder xs => EncodingInfo xs -> NP I xs -> K EncodingAction xs
      gUnparse' (EncodeZero n) Nil = K $ toEncoding $ T.toLower $ T.pack  n
      gUnparse' (EncodeSingle t) (I a :* Nil) = K $ toEncoding t <> toEncoding a
      gUnparse' (EncodeMultiple tag) cs = K $ tagWith tag $ hcollapse . hcliftA allP (K . toEncoding . unI) $ cs
      gUnparse' (EncodeRecord tag fields) cs = K . tagWith tag . hcollapse . hcliftA allP (K . toEncoding . unI) $ cs

      allP = Proxy :: Proxy GuacamoleEncoder
      allFP = Proxy :: Proxy (All GuacamoleEncoder)

      tagWith :: Tag -> [EncodingAction] -> EncodingAction
      tagWith t eas = toEncoding t <> mconcat eas
      -- tagWith t eas = toEncoding t <> mconcat eas

-- black magic ends here

-- For tag encoding
instance GuacamoleEncoder Tag where
    toEncoding NoTag     = mempty
    toEncoding (Tag tag) = toEncoding tag



encodeActions :: EncodingAction -> Either Text GuacamolePacket
encodeActions xs =  encodeActions' xs >>= \(bs:args) -> Right $ GuacamolePacket bs args

encodeActions' :: EncodingAction -> Either Text [ByteString]
encodeActions' eas = case eas of
                      Plain good -> Right $ map (BSL.toStrict . BE.toLazyByteString) good
                      Error bad  -> Left bad


encodeChunk :: BE.Builder -> EncodingAction
encodeChunk b = Plain [ b ]

instance GuacamoleEncoder GuacamoleXY
instance GuacamoleEncoder GuacamoleSize
instance GuacamoleEncoder GuacamoleRGBA
instance GuacamoleEncoder TransformLayer
instance GuacamoleEncoder PipeRequest
instance GuacamoleEncoder PipeRequestNamed
instance GuacamoleEncoder PipeRequestIndexed
instance GuacamoleEncoder PipeStatus
instance GuacamoleEncoder PipeBlob
instance GuacamoleEncoder PipeObject
instance GuacamoleEncoder GuacamoleCompositeMode
instance GuacamoleEncoder GuacamoleCapStyle
instance GuacamoleEncoder GuacamoleJoinStyle
instance GuacamoleEncoder GuacamoleTransfer

instance GuacamoleEncoder [ByteString] where
    toEncoding = foldMap toEncoding

instance GuacamoleEncoder Bool where
    toEncoding  = toEncoding . bool2int
        where
            bool2int :: Bool -> Int
            bool2int False = 0
            bool2int True  = 0

instance GuacamoleEncoder Int where
    toEncoding  = encodeChunk . BE.intDec

instance GuacamoleEncoder Integer where
    toEncoding = encodeChunk . BE.integerDec

instance GuacamoleEncoder Float where
    toEncoding = encodeChunk . BE.floatDec

instance GuacamoleEncoder Double where
    toEncoding = encodeChunk . BE.doubleDec

instance GuacamoleEncoder ByteString where
    toEncoding = encodeChunk . BE.byteString

instance GuacamoleEncoder BSL.ByteString where
    toEncoding = encodeChunk . BE.lazyByteString

instance GuacamoleEncoder Text where
    toEncoding = encodeChunk . BE.byteString . T.encodeUtf8

instance GuacamoleEncoder GuacamoleStatusEnum where
    toEncoding = toEncoding . fromGuacamoleEnum
