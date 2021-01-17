{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


module Game (
  GameSchema
, GuessParams(..)
, LockParams(..)
, clearString
, game
, gameAddress
, gameValidator
, guess
, lock
, hashString
, validateGuess
) where


import Language.PlutusTx.Prelude

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Language.Plutus.Contract (AsContractError, BlockchainActions, Contract, Endpoint, type (.\/), collectFromScript, endpoint, select, submitTxConstraints, submitTxConstraintsSpending, submitTxConstraintsSpending, utxoAt)
import Ledger (Address, ValidatorCtx, Validator, Value)

import qualified Data.ByteString.Char8 as C
import qualified Language.PlutusTx     as PlutusTx
import qualified Ledger                as Ledger
import qualified Ledger.Constraints    as Constraints
import qualified Ledger.Typed.Scripts  as Scripts
import qualified Prelude


newtype HashedString = HashedString ByteString
  deriving newtype PlutusTx.IsData

PlutusTx.makeLift ''HashedString


newtype ClearString = ClearString ByteString
  deriving newtype PlutusTx.IsData

PlutusTx.makeLift ''ClearString


type GameSchema =
      BlockchainActions
  .\/ Endpoint "lock" LockParams
  .\/ Endpoint "guess" GuessParams


-- | The validation function (DataValue -> RedeemerValue -> ValidatorCtx -> Bool)
validateGuess :: HashedString -> ClearString -> ValidatorCtx -> Bool
validateGuess (HashedString actual) (ClearString guess') _ = actual == sha2_256 guess'


-- | The validator script of the game.
gameValidator :: Validator
gameValidator = Scripts.validatorScript gameInstance


data Game

instance Scripts.ScriptType Game where
  type instance RedeemerType Game = ClearString
  type instance DatumType Game = HashedString


gameInstance :: Scripts.ScriptInstance Game
gameInstance = Scripts.validator @Game
  $$(PlutusTx.compile [|| validateGuess ||])
  $$(PlutusTx.compile [|| wrap ||])
    where
      wrap = Scripts.wrapValidator @HashedString @ClearString


-- create a data script for the guessing game by hashing the string
-- and lifting the hash to its on-chain representation
hashString :: String -> HashedString
hashString = HashedString . sha2_256 . C.pack


-- create a redeemer script for the guessing game by lifting the
-- string to its on-chain representation
clearString :: String -> ClearString
clearString = ClearString . C.pack


-- | The address of the game (the hash of its validator script)
gameAddress :: Address
gameAddress = Ledger.scriptAddress gameValidator


-- | Parameters for the "lock" endpoint
data LockParams = LockParams {
  secretWord :: String
, amount     :: Value
}
  deriving stock (Prelude.Eq, Prelude.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


--  | Parameters for the "guess" endpoint
newtype GuessParams = GuessParams {
  guessWord :: String
}
  deriving stock (Prelude.Eq, Prelude.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


lock :: AsContractError e => Contract GameSchema e ()
lock =
  do
    LockParams secret amt <- endpoint @"lock" @LockParams
    let tx = Constraints.mustPayToTheScript (hashString secret) amt
    void (submitTxConstraints gameInstance tx)


guess :: AsContractError e => Contract GameSchema e ()
guess =
 do
   GuessParams theGuess <- endpoint @"guess" @GuessParams
   unspentOutputs <- utxoAt gameAddress
   let redeemer = clearString theGuess
       tx       = collectFromScript unspentOutputs redeemer
   void (submitTxConstraintsSpending gameInstance unspentOutputs tx)


game :: AsContractError e => Contract GameSchema e ()
game = lock `select` guess
