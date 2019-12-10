{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SAWServer.ProofScript
  ( ProofScript
  , interpretProofScript
  ) where

import Control.Applicative
import Data.Aeson (FromJSON(..), withObject, (.:))

import qualified SAWScript.Builtins as SB
import qualified SAWScript.Value as SV

data Prover
  = ABC
  | CVC4 [String]
  | RME
  | Yices [String]
  | Z3 [String]

data ProofTactic
  = UseProver Prover
  | Unfold [String]
  | BetaReduceGoal
  | EvaluateGoal [String]
  | AssumeUnsat
  | Trivial

newtype ProofScript = ProofScript [ProofTactic]

instance FromJSON Prover where
  parseJSON =
    withObject "prover" $ \o -> do
      (name :: String) <- o .: "name"
      case name of
        "abc"   -> pure ABC
        "cvc4"  -> CVC4 <$> o .: "uninterpreted functions"
        "rme"   -> pure RME
        "yices" -> Yices <$> o .: "uninterpreted functions"
        "z3"    -> Z3 <$> o .: "uninterpreted functions"
        _       -> empty

instance FromJSON ProofTactic where
  parseJSON =
    withObject "proof tactic" $ \o -> do
      (tac :: String) <- o .: "tactic"
      case tac of
        "use prover"       -> UseProver <$> o .: "prover"
        "unfold"           -> Unfold <$> o .: "names"
        "beta reduce goal" -> pure BetaReduceGoal
        "evalute goal"     -> EvaluateGoal <$> o .: "uninterpreted functions"
        "assume unsat"     -> pure AssumeUnsat
        "trivial"          -> pure Trivial
        _                  -> empty

instance FromJSON ProofScript where
  parseJSON =
    withObject "proof script" $ \o -> ProofScript <$> o .: "tactics"


interpretProofScript :: ProofScript -> SV.ProofScript SV.SatResult
interpretProofScript (ProofScript ts) = go ts
  where go [UseProver ABC]            = SB.satABC
        go [UseProver (CVC4 unints)]  = SB.satWhat4_UnintCVC4 unints
        go [UseProver RME]            = SB.satRME
        go [UseProver (Yices unints)] = SB.satWhat4_UnintYices unints
        go [UseProver (Z3 unints)]    = SB.satWhat4_UnintZ3 unints
        go [Trivial]                  = SB.trivial
        go [AssumeUnsat]              = SB.assumeUnsat
        go (BetaReduceGoal : rest)    = SB.beta_reduce_goal >> go rest
        go (EvaluateGoal fns : rest)  = SB.goal_eval fns    >> go rest
        go (Unfold fns : rest)        = SB.unfoldGoal fns   >> go rest
        go _ = fail "malformed proof script"
