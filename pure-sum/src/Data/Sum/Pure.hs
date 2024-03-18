{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module        : Data.Sum.Pure
-- Copyright     : Gautier DI FOLCO
-- License       : BSD2
--
-- Maintainer    : Gautier DI FOLCO <gautier.difolco@gmail.com>
-- Stability     : Unstable
-- Portability   : GHC
--
-- Derive fromText/toText-like for pure sum types.
module Data.Sum.Pure
  ( -- * Base type
    PureSumWith (..),
    PureSum,

    -- * from/to Text converters
    ToSumText (..),
    FromSumText (..),

    -- * Transformations
    Transformation (..),
    type (<<<),
    DropPrefix,
    CamelCase,
    PascalCase,
    SnakeCase,
    SpinalCase,
    TitleCase,
    TrainCase,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Kind
import Data.Maybe (fromMaybe)
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Manipulate as Manipulation
import GHC.Generics
import GHC.TypeError
import GHC.TypeLits

-- * Base interface

-- | Wrapper for derivation.
-- @transformation@ is a @Transformation@ applied during derivations
newtype PureSumWith transformation a = PureSumWith {unPureSumWith :: a}
  deriving stock (Eq, Ord, Show)

-- | Basic sum derivation
type PureSum = PureSumWith IdTransformation

class ToSumText a where
  toSumText :: a -> T.Text

class FromSumText a where
  fromSumText :: T.Text -> Maybe a

-- * Text Functions

-- | Convert a type into a @Text -> Text@ function
class Transformation a where
  transform :: Proxy a -> T.Text -> T.Text

-- | Apply no transformation (like @id@)
data IdTransformation

instance Transformation IdTransformation where
  transform _ = id

-- | Compose two transformations (e.g. @f << g@ is equivalent to @f (g x)@)
data f <<< g

instance (Transformation f, Transformation g) => Transformation (f <<< g) where
  transform _ = transform (Proxy @f) . transform (Proxy @g)

-- | @DropPrefix prefix@ (e.g. @DropPrefix "A"@ on "ACase" gives "Case")
data DropPrefix (s :: Symbol)

instance (KnownSymbol s) => Transformation (DropPrefix s) where
  transform _ x = fromMaybe x $ T.stripPrefix (T.pack $ symbolVal @s Proxy) x

-- | Change case (e.g. "camelCasedPhrase")
data CamelCase

instance Transformation CamelCase where
  transform _ = Manipulation.toCamel

-- | Change case (e.g. "PascalCasedPhrase")
data PascalCase

instance Transformation PascalCase where
  transform _ = Manipulation.toPascal

-- | Change case (e.g. "snake_cased_phrase")
data SnakeCase

instance Transformation SnakeCase where
  transform _ = Manipulation.toSnake

-- | Change case (e.g. "spinal-cased-phrase")
data SpinalCase

instance Transformation SpinalCase where
  transform _ = Manipulation.toSpinal

-- | Change case (e.g. "Title Cased Phrase")
data TitleCase

instance Transformation TitleCase where
  transform _ = Manipulation.toTitle

-- | Change case (e.g. "Train-Cased-Phrase")
data TrainCase

instance Transformation TrainCase where
  transform _ = Manipulation.toTrain

-- * Generic derivation

-- * ToSumText

instance
  (Transformation transformation, Generic a, GToSumText (Rep a)) =>
  ToSumText (PureSumWith transformation a)
  where
  toSumText = gToSumText (transform $ Proxy @transformation) . from . unPureSumWith

class GToSumText f where
  gToSumText :: (T.Text -> T.Text) -> f a -> T.Text

instance (GToSumText a) => GToSumText (M1 D meta a) where -- base type
  gToSumText transformation (M1 x) = gToSumText transformation x

instance (KnownSymbol cntr, EnsureEmpty a) => GToSumText (M1 C ('MetaCons cntr p b) a) where -- constructor
  gToSumText transformation (M1 _) = transformation $ T.pack $ symbolVal $ Proxy @cntr

instance (GToSumText a, GToSumText b) => GToSumText (a :+: b) where -- sum type
  gToSumText transformation (R1 x) = gToSumText transformation x
  gToSumText transformation (L1 x) = gToSumText transformation x

instance (NonSumTypeError) => GToSumText V1 where
  gToSumText _ _ = error "impossible"

-- * FromSumText

instance
  (Transformation transformation, Generic a, GFromSumText (Rep a)) =>
  FromSumText (PureSumWith transformation a)
  where
  fromSumText x = PureSumWith . to <$> gFromSumText (transform $ Proxy @transformation) x

class GFromSumText f where
  gFromSumText :: (T.Text -> T.Text) -> T.Text -> Maybe (f a)

instance
  (GFromSumText a) =>
  GFromSumText (M1 D ('MetaData typeName c i b) a) -- base type
  where
  gFromSumText transformation x = M1 <$> gFromSumText transformation x

instance
  (GFromSumText a, KnownSymbol cntr) =>
  GFromSumText (M1 C ('MetaCons cntr p b) a) -- constructor
  where
  gFromSumText transformation v =
    M1 <$> do
      let cntrName = transformation $ T.pack $ symbolVal (Proxy @cntr)
      guard $ v == cntrName
      gFromSumText transformation v

instance (GFromSumText a, GFromSumText b) => GFromSumText (a :+: b) where -- sumtype
  gFromSumText transformation v =
    (L1 <$> gFromSumText transformation v)
      <|> (R1 <$> gFromSumText transformation v)

instance GFromSumText U1 where
  gFromSumText _ _ = return U1

instance (NonSumTypeError) => GFromSumText V1 where
  gFromSumText _ _ = error "impossible"

instance (NonSumTypeError) => GFromSumText (M1 S meta a) where
  gFromSumText _ _ = error "impossible"

-- * Utils

type family EnsureEmpty a :: Constraint where
  EnsureEmpty U1 = ()
  EnsureEmpty a = NonSumTypeError

type NonSumTypeError :: Constraint
type NonSumTypeError = TypeError (Text "Only pure sum types are supported (constructor(s) without values)")
