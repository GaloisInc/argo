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

class Described a where
  typeName :: Text
  description :: [Block]

class DescribedParams a where
  parameterFieldDescription :: [(Text, Block)]

datatype :: forall a . (Typeable a, Described a) => Block
datatype =
  Datatype (typeRep (Proxy @a)) (typeName @a) (description @a)
