{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module Notary where


import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Language.Plutus.Contract
import Language.Plutus.Contract (AsContractError, BlockchainActions, Contract, Endpoint, type (.\/), collectFromScript, endpoint, select, submitTxConstraints, submitTxConstraintsSpending, submitTxConstraintsSpending, utxoAt)
import Language.PlutusTx.Prelude  hiding (Applicative (..))
import Ledger (Address, Datum(..), PubKeyHash, ValidatorCtx, scriptAddress, txOutTxDatum)
import Ledger.Constraints.TxConstraints (mustIncludeDatum, mustValidateIn)
import Ledger.Value (Value)
import Numeric (showHex)
import Data.ByteString.Base64.URL (encodeBase64)

import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Language.PlutusTx    as PlutusTx
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Interval      as Interval
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Validation as V
import qualified Prelude



newtype NotaryService =
  Service
  {
    notaryOwner :: PubKeyHash
  }
    deriving (Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''NotaryService


data Notarization =
  Notarization
  {
    hash        :: ByteString
  , description :: C.ByteString
  }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeIsData ''Notarization


notaryAddress :: NotaryService -> Address
notaryAddress = Ledger.scriptAddress . Scripts.validatorScript . notaryInstance


data Notary

instance Scripts.ScriptType Notary where
  type instance RedeemerType Notary = ()
  type instance DatumType    Notary = Notarization


notaryInstance :: NotaryService -> Scripts.ScriptInstance Notary
notaryInstance service =
  Scripts.validator @Notary
    ($$(PlutusTx.compile [|| validatePublish ||]) `PlutusTx.applyCode` PlutusTx.liftCode service)
    $$(PlutusTx.compile [|| wrap            ||])
      where
        wrap = Scripts.wrapValidator @Notarization @()


validatePublish :: NotaryService -> Notarization -> () -> ValidatorCtx -> Bool
validatePublish Service{..} _ () txinfo =
  valCtxTxInfo txinfo `V.txSignedBy` notaryOwner


type NotarySchema =
  BlockchainActions
    .\/ Endpoint "notarize" (Notarization, Value)
    .\/ Endpoint "publish"  ()


notary :: AsContractError e => NotaryService -> Contract NotarySchema e ()
notary service = notarize service `select` publish service


notarize :: AsContractError e => NotaryService -> Contract NotarySchema e ()
notarize service =
  do
    (notarization, lockedFunds) <- endpoint @"notarize" @(Notarization, Value)
    let
      tx = Constraints.mustPayToTheScript notarization lockedFunds
    void $ submitTxConstraints (notaryInstance service) tx


publish :: AsContractError e => NotaryService -> Contract NotarySchema e ()
publish service =
  let
    go =
      do
        unspentOutputs <- utxoAt $ notaryAddress service
        sequence_
          [
            let
              unspentOutput = M.singleton outref out
              Just out' = txOutTxDatum out
              Just Notarization{..} = PlutusTx.fromData $ getDatum out'
              url = Datum . PlutusTx.toData $ C.pack $ "http://notary.io/" ++ T.unpack (encodeBase64 hash)
              tx = collectFromScript unspentOutput () <> mustIncludeDatum url
            in
              void $ submitTxConstraintsSpending (notaryInstance service) unspentOutputs tx
          |
            (outref, out) <- M.toList unspentOutputs
          ] 
        waitNSlots 1
        go
  in
    do
      () <- endpoint @"publish" @()
      go
