{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Argo.Doc (LinkTarget(..), Block(..), Inline(..), Described(..), DescribedParams(..), datatype) where

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


-- | This class provides the canonical documentation for a datatype
-- that is deserialized as parameters to some method via a @FromJSON@
-- instance. The type variable does not occur in the method's
-- signature because it is intended to be used with the
-- @TypeApplications@ extension to GHC Haskell.
class DescribedParams a where
  parameterFieldDescription :: [(Text, Block)]

datatype :: forall a . (Typeable a, Described a) => Block
datatype =
  Datatype (typeRep (Proxy @a)) (typeName @a) (description @a)
