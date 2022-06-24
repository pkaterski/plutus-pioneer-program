{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module Week04.ContractTest where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import Data.Void                  (Void)
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

-- import Data.Default               (Default (..))
-- import Ledger.TimeSlot
-- import Ledger

-- import Week04.Vesting


-- Contract w s e a
-- w - writer, to communicate b/ different contracts
-- s - endpoints
-- e - error messages
-- a - result

myContract1 :: Contract () Empty Text ()
myContract1 = do
  void $ Contract.throwError "Lele, pred curkvata nqkoi ostavil e bebe"
  Contract.logInfo @String "Opa"

myTrace1 :: EmulatorTrace ()
myTrace1 = void $ activateContractWallet (knownWallet 1) myContract1


myContract2 :: Contract () Empty Void ()
myContract2 = Contract.handleError (\err ->
  Contract.logError @String $ "O nee: " ++ unpack err)
  myContract1

myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (knownWallet 1) myContract2

type MySchema = Endpoint "e1" Int .\/ Endpoint "e2" String

myContract3 :: Contract () MySchema Text ()
myContract3 = do
  awaitPromise $ endpoint @"e1" Contract.logInfo
  awaitPromise $ endpoint @"e2" Contract.logInfo

myTrace3 :: EmulatorTrace ()
myTrace3 = do
  h <- activateContractWallet (knownWallet 1) myContract3
  callEndpoint @"e1" h 69
  callEndpoint @"e2" h "Olele"

myContract4 :: Contract [Int] Empty Text ()
myContract4 = do
  void $ Contract.waitNSlots 10
  tell [1]
  void $ Contract.waitNSlots 10
  tell [2]
  void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do
  h <- activateContractWallet (knownWallet 1) myContract4
  void $ Emulator.waitNSlots 5
  xs <- observableState h
  Extras.logInfo $ show xs
  void $ Emulator.waitNSlots 10
  ys <- observableState h
  Extras.logInfo $ show ys
  void $ Emulator.waitNSlots 10
  zs <- observableState h
  Extras.logInfo $ show zs

