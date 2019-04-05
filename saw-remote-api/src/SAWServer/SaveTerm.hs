{-# LANGUAGE OverloadedStrings #-}
module SAWServer.SaveTerm where

import Control.Applicative
import Control.Lens
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.=), (.:))

import Verifier.SAW.CryptolEnv

import Argo

import CryptolServer.Data.Expression
import SAWServer
import SAWServer.OK

saveTerm :: SaveTermParams -> Method SAWState OK
saveTerm (SaveTermParams name envName e) =
  do expr <- getExpr e
     sc <- view sawSC <$> getState
     env <- getCryptolEnv envName
     ok


      -- SS.Code str            -> do sc <- getSharedContext
      --                              cenv <- fmap rwCryptol (getMergedEnv env)
      --                              --io $ putStrLn $ "Parsing code: " ++ show str
      --                              --showCryptolEnv' cenv
      --                              t <- io $ CEnv.parseTypedTerm sc cenv
      --                                      $ locToInput str
      --                              return (toValue t)

data SaveTermParams =
  SaveTermParams
    { termName :: ServerName
    , termEnv :: ServerName
    , termExpr :: Expression
    }

instance FromJSON SaveTermParams where
  parseJSON =
    withObject "parameters for saving a term" $ \o ->
    SaveTermParams <$> o .: "name"
                   <*> o .: "environment"
                   <*> o .: "expression"
