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

module Network.Guacamole.Types.SOP (
                                     EncodingInfo(..)
                                   , Tag(..)
                                   , encodingInfo
                                   , EncodingOptions(..)
                                   , defaultEncodingOptions
    ) where

import           Universum hiding (All, Generic)

import qualified Data.ByteString.Char8 as BS8
import           Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Generics.SOP (All, All2, Code, ConstructorInfo (..), ConstructorName,
                               DatatypeInfo (..), DatatypeName, FieldInfo (..), FieldName (..),
                               HasDatatypeInfo (..), I (..), K (..), NP (..), NS (..), SListI,
                               SOP (..), Shape (..), hliftA, shape)


data EncodingInfo :: [*] -> * where
    EncodeZero :: ConstructorName -> EncodingInfo '[]
    EncodeSingle :: Tag -> EncodingInfo '[a]
    EncodeMultiple :: SListI xs => Tag -> EncodingInfo xs
    EncodeRecord :: SListI xs => Tag -> NP (K String) xs -> EncodingInfo xs

-- | ex-JSON encoder/decoder configuration
data EncodingOptions = EncodingOptions {
    -- | Construct the name for JSON object fields (not for the tags that are
    -- used for sum-types, however)
    --
    -- The default just uses the name of the corresponding Haskell constructor
    encodeFieldName :: DatatypeName -> FieldName -> String

    -- | Construct the name for a tag for sum-types.
    --
    -- The default just uses the name of the Haskell constructor.
  , encodeTagName   :: ConstructorName -> Text
  }

defaultEncodingOptions :: EncodingOptions
defaultEncodingOptions = EncodingOptions {
    encodeFieldName = const identity
  , encodeTagName   = T.toLower . T.pack
  }


data Tag = NoTag | Tag Text

encodingInfo :: forall a. (HasDatatypeInfo a, SListI (Code a))
             => Proxy a -> EncodingOptions -> NP EncodingInfo (Code a)
encodingInfo pa opts =
  case datatypeInfo pa of
    Newtype _ _ _  -> EncodeSingle NoTag :* Nil
    ADT     _ n cs -> hliftA (encodingInfoFor opts n (tag cs)) cs
  where
    tag :: NP ConstructorInfo (Code a) -> ConstructorName -> Tag
    tag cs | _ :* Nil <- cs = const NoTag
           | otherwise      = Tag . encodeTagName opts

encodingInfoFor :: forall xs. EncodingOptions -> DatatypeName -> (ConstructorName -> Tag) -> ConstructorInfo xs -> EncodingInfo xs
encodingInfoFor _    _ tag (Infix n _ _)   = EncodeMultiple (tag n)
encodingInfoFor _    _ tag (Constructor n) =
  case shape :: Shape xs of
    ShapeNil           -> EncodeZero     n
    ShapeCons ShapeNil -> EncodeSingle   (tag n)
    _                  -> EncodeMultiple (tag n)
encodingInfoFor opts dn tag (Record n fields) =
                          EncodeRecord (tag n) (hliftA fieldName' fields)
  where
    fieldName' :: FieldInfo a -> K String a
    fieldName' (FieldInfo name) = K (encodeFieldName opts dn name)
