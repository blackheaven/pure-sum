{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module        : Data.Sum.Pure.Aeson
-- Copyright     : Gautier DI FOLCO
-- License       : BSD2
--
-- Maintainer    : Gautier DI FOLCO <gautier.difolco@gmail.com>
-- Stability     : Unstable
-- Portability   : GHC
--
-- Derive fromText/toText-like for pure sum types (aeson instances).
module Data.Sum.Pure.Aeson
  ( -- * Base type
    FromJSON (..),
    FromJSONKey (..),
    ToJSON (..),
    ToJSONKey (..),

    -- * Reexport
    module X,
  )
where

import Data.Aeson.Types
import Data.Proxy
import Data.Sum.Pure as X
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits

-- -- * Base interface
--
-- -- | Wrapper for derivation.
-- -- @transformation@ is a @Transformation@ applied during derivations
-- newtype PureSumWith transformation a = PureSumWith {unPureSumWith :: a}
--   deriving stock (Eq, Ord, Show)
--
-- -- | Basic sum derivation
-- type PureSum = PureSumWith IdTransformation
--
-- class ToSumText a where
--   toSumText :: a -> T.Text
--
-- class FromSumText a where
--   fromSumText :: T.Text -> Maybe a

instance (FromSumText a, Generic a, GConstructorName (Rep a)) => FromJSON (PureSumWith transformation a) where
  parseJSON = withText (unConstructionName $ (to @a) <$> gConstructorName) pureSumWithParser

instance (FromSumText a, Generic a, GConstructorName (Rep a)) => FromJSONKey (PureSumWith transformation a) where
  fromJSONKey = FromJSONKeyTextParser pureSumWithParser

pureSumWithParser :: (FromSumText a) => T.Text -> Parser (PureSumWith transformation a)
pureSumWithParser x =
  maybe (fail $ "unknown value: " <> show x) (pure . PureSumWith) $
    fromSumText x

newtype ConstructorName x = ConstructorName {unConstructionName :: String}

instance Functor ConstructorName where
  fmap _ (ConstructorName x) = ConstructorName x

class GConstructorName f where
  gConstructorName :: ConstructorName (f a)

instance (KnownSymbol typeName) => GConstructorName (M1 D ('MetaData typeName c i b) a) where -- base type
  gConstructorName = ConstructorName $ symbolVal (Proxy @typeName)

instance (ToSumText a) => ToJSON (PureSumWith transformation a) where
  toJSON = toJSON . toSumText . unPureSumWith
  toEncoding = toEncoding . toSumText . unPureSumWith

instance (ToSumText a) => ToJSONKey (PureSumWith transformation a) where
  toJSONKey = toJSONKeyText (toSumText . unPureSumWith)
