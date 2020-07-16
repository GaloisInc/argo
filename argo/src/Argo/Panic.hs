{-# LANGUAGE TemplateHaskell #-}

-- | Provides a means of crashing a program when supposedly-impossible
-- situations arise.
module Argo.Panic (HasCallStack, panic) where

import Panic hiding (panic)
import qualified Panic as Panic

data Argo = Argo

type ArgoPanic = Panic Argo

-- | Crash the program with a message.
panic ::
  HasCallStack =>
  String {-^ The program site to identify as the source of the panic -} ->
  [String] {-^ Further descriptions of why the panic is occurring -} ->
  a
panic = Panic.panic Argo

instance PanicComponent Argo where
  panicComponentName _ = "Argo"
  panicComponentIssues _ = "https://github.com/GaloisInc/argo/issues"

  {-# Noinline panicComponentRevision #-}
  panicComponentRevision = $useGitRevision
