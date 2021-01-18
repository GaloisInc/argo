{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Argo.Doc.ReST (restructuredText) where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Char
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import Numeric.Natural

import Argo.Doc

data ReSTContext =
  ReSTContext
  { sectionNestingLevel :: Natural
  , textIndentLevel :: Natural
  }


restructuredText :: Block -> Text
restructuredText block =
  execWriter $ runReaderT (go block) $ ReSTContext 0 0
  where
    go :: Block -> ReaderT ReSTContext (Writer Text) ()
    go (Section name contents) =
      do header name
         terpri
         nestSection (traverse_ go contents)
         terpri
    go (Datatype t name contents) =
      do anchor (mangle (show t))
         header name
         terpri
         nestSection (traverse_ go contents)
         terpri
    go (App name contents) = go (Section name contents)
    go (Paragraph contents) =
      do traverse_ inline contents
         terpri >> terpri
    go (DescriptionList elts) =
      for_ elts $ \(name, what) ->
        do terpri
           traverse_ inline name
           indented $ terpri >> go what
           terpri
    go (BulletedList elts) =
      for_ elts $
        \contents ->
          do terpri
             tell "* "
             indented (go contents)
             terpri

    nestSection ::
      ReaderT ReSTContext (Writer Text) a ->
      ReaderT ReSTContext (Writer Text) a
    nestSection =
      local $ \(ReSTContext lvl indent) -> ReSTContext (lvl + 1) indent

    indented ::
      ReaderT ReSTContext (Writer Text) a ->
      ReaderT ReSTContext (Writer Text) a
    indented =
      local $ \(ReSTContext lvl indent) -> ReSTContext lvl (indent + 2)

    inline :: Inline -> ReaderT ReSTContext (Writer Text) ()
    inline (Text txt) = traverse_ tell $ T.lines txt
    inline (Link tgt label) =
      case tgt of
        URL url -> tell $ "`" <> label <> " <" <> url <> ">`_"
        TypeDesc t -> tell $ ":ref:`" <> label <> " <" <> mangle (show t) <> ">`"
    inline (Literal txt) = tell $ "``" <> txt <> "``"

    terpri :: ReaderT ReSTContext (Writer Text) ()
    terpri =
      do i <- asks textIndentLevel
         tell "\n"
         tell (T.replicate (fromIntegral i) " ")

    anchor :: Text -> ReaderT ReSTContext (Writer Text) ()
    anchor name = tell $ ".. _" <> name <> ":\n"

    mangle :: String -> Text
    mangle [] = ""
    mangle (c:cs)
      | isAlphaNum c = T.singleton c <> mangle cs
      | c == '-' = "_dash_" <> mangle cs
      | c == '>' = "_greater_" <> mangle cs
      | c == '<' = "_less_" <> mangle cs
      | c == '(' = "_lpar_" <> mangle cs
      | c == ')' = "_rpar_" <> mangle cs
      | c == ' ' = "_space_" <> mangle cs
      | c == ':' = "_colon_" <> mangle cs
      | c == '~' = "_twiddle_" <> mangle cs
      | c == '[' = "_lsq_" <> mangle cs
      | c == ']' = "_rsq_" <> mangle cs
      | c == '*' = "_splat_" <> mangle cs
      | otherwise = "___" <> mangle cs


    header :: Text -> ReaderT ReSTContext (Writer Text) ()
    header name =
      do tell name
         terpri
         c <- headerChar
         tell $ T.replicate (T.length name) (T.singleton c)
         terpri

    headerChar =
      do lvl <- asks sectionNestingLevel
         pure (getC lvl "=-~+*#`_")
      where getC _ [] = error "Nesting too deep in docs"
            getC n (c:cs)
              | n == 0 = c
              | otherwise = getC (n - 1) cs
