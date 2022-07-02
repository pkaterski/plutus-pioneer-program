{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}

module Week03.VestingTemp where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text, unpack)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show(..), String)
import           Text.Printf          (printf)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

data VestingDatum = VestingDatum
  { beneficiary :: PaymentPubKeyHash
  , deadline    :: POSIXTime
  } deriving Show

PlutusTx.unstableMakeIsData ''VestingDatum

{-# INLINABLE mkValidator #-}
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                         traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ beneficiary dat

    -- the transaction seems to be submitted when it reaches its valid interval
    -- so it seems that this cannot be exploted by providing an interval far into the future
    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info


data Typed
instance Scripts.ValidatorTypes Typed where
  type instance DatumType Typed = VestingDatum
  type instance RedeemerType Typed = ()

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
  $$(PlutusTx.compile [|| mkValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
    where
      wrap = Scripts.wrapValidator @VestingDatum @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

data GiveParams = GiveParams
  { gpBeneficiary :: !PaymentPubKeyHash
  , gpDeadline    :: !POSIXTime
  , gpAmount      :: !Integer
  } deriving (Generic, ToJSON, FromJSON, ToSchema)

type GiftSchema =
            Endpoint "give" GiveParams
        .\/ Endpoint "grab" ()
        .\/ Endpoint "grabFuture" ()
        .\/ Endpoint "grabHoly" ()

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
  let dat = VestingDatum
              { beneficiary = gpBeneficiary gp
              , deadline    = gpDeadline gp
              }
      tx = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ gpAmount gp
  ledgerTx <- submitTxConstraints typedValidator tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String $ printf "made a gift of %d lovelace with deadline %s"
    (gpAmount gp)
    (show $ gpDeadline gp)

-- doesn't filter the valid utxos but provied validity interval from now to ∞
grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    now   <- currentTime
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                 Constraints.mustValidateIn (from now)
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "collected gifts"

-- tries to grab funds and sets its own validity interval from 10 slots in the future
grabFuture :: forall w s e. AsContractError e => Contract w s e ()
grabFuture = do
    now   <- currentTime
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                      -- submits a transaction to be executed in the future
                      -- too far in the future may be forbidden?
                      Constraints.mustValidateIn (from $ now + 10000)
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "collected gifts"

-- searches only for valid utxos (with valid signature and deadline passed)
-- and only tries to spend them
grabHoly :: forall w s e. AsContractError e => Contract w s e ()
grabHoly = do
    now   <- currentTime
    pkh   <- ownPaymentPubKeyHash
    utxos <- Map.filter (isSuitable pkh now) <$> utxosAt scrAddress
    if Map.null utxos
       then logInfo @String $ "no gifts available"
       else do
         let orefs   = fst <$> Map.toList utxos
             lookups = Constraints.unspentOutputs utxos      <>
                       Constraints.otherScript validator
             tx :: TxConstraints Void Void
             tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                      Constraints.mustValidateIn (from now)
         ledgerTx <- submitTxConstraintsWith @Void lookups tx
         void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
         logInfo @String $ "collected gifts"
  where
    isSuitable :: PaymentPubKeyHash -> POSIXTime -> ChainIndexTxOut -> Bool
    isSuitable pkh now o = case _ciTxOutDatum o of
      Left _          -> False
      Right (Datum e) -> case PlutusTx.fromBuiltinData e of
        Nothing -> False
        Just d  -> beneficiary d == pkh && deadline d <= now

-- if an endpoints throws an error all the other endpoints might be blocked?
-- because it would never reach the `>> endpoints` in the Contract monad
endpoints' :: Contract () GiftSchema Text ()
endpoints' = awaitPromise (((give' `select` grab') `select` grabFuture') `select` grabHoly') >> endpoints'
  where
    grab'       = endpoint @"grab" $ const grab
    give'       = endpoint @"give" give
    grabFuture' = endpoint @"grabFuture" $ const grabFuture
    grabHoly'   = endpoint @"grabHoly" $ const grabHoly

instance AsContractError Void

-- handle errors so that all the endpoints don't get blocked because of a previous error
endpoints :: Contract () GiftSchema Text ()
endpoints = do
    void $ awaitPromise $ give' `select` grab' `select` grabFuture' `select` grabHoly'
    endpoints
  where
    noError     = handleError (\err -> logError @String $ "ERROR: " ++ unpack err)
    grab'       = endpoint @"grab"       $ noError . const grab
    give'       = endpoint @"give"       $ noError . give
    grabFuture' = endpoint @"grabFuture" $ noError . const grabFuture
    grabHoly'   = endpoint @"grabHoly"   $ noError . const grabHoly

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []

