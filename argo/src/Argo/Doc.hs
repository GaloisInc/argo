{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Argo.Doc (LinkTarget(..), Block(..), Inline(..), Described(..), DescribedMethod(..), datatype) where

import Data.List.NonEmpty
import Data.Text (Text)
import Data.Typeable

data LinkTarget
  = URL Text
  | TypeDesc TypeRep

data Block
  = Section Text [Block]
  | Datatype TypeRep Text [Block]
  | App Text [Block]
  | Paragraph [Inline]
  | DescriptionList [(NonEmpty Inline, Block)]
  | BulletedList [Block]

data Inline
  = Text Text
  | Link LinkTarget Text
  | Literal Text

-- | This class provides the canonical documentation for a datatype
-- that occurs as part of a protocol message (in other words, it
-- documents the @FromJSON@ and @ToJSON@ instances). The type variable
-- does not occur in the method's signature because it is intended to
-- be used with the @TypeApplications@ extension to GHC Haskell.
class Described a where
  typeName :: Text
  description :: [Block]


-- | This class provides the canonical documentation for a pair of datatypes,
-- where:
--
-- * The first datatype (@params@) is deserialized as parameters to some method
--   via a @FromJSON@ instance, and
--
-- * The second datatype (@result@) is serialized as the result returned by the
--   same method via a @ToJSON@ instance.
--
-- The @params@ type is almost always a custom data type defined for the
-- purpose of interfacing with RPC, which is why @result@ has a functional
-- dependency on @params@. On the other hand, it is common for @result@ to be
-- off-the-shelf data types such as @Value@ (for methods that return a JSON
-- object) or @()@ (for methods that do not return any values).
--
-- Neither @params@ nor @result@ occur in the signatures of the methods
-- because they are intended to be used with the @TypeApplications@ extension
-- to GHC Haskell.
class DescribedMethod params result | params -> result where
  -- | Documentation for the parameters expected by the method.
  parameterFieldDescription :: [(Text, Block)]

  -- | Documentation for the result returned by the method.
  --
  -- If the method does not return anything—that is, if @result@ is
  -- @()@—then this method does not need to be implemented, as it will be
  -- defaulted appropriately.
  resultFieldDescription :: [(Text, Block)]
  default resultFieldDescription :: (result ~ ()) => [(Text, Block)]
  resultFieldDescription = []

datatype :: forall a . (Typeable a, Described a) => Block
datatype =
  Datatype (typeRep (Proxy @a)) (typeName @a) (description @a)
