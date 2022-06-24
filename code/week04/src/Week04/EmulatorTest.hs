{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}

module Week04.EmulatorTest where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger
import Ledger.TimeSlot
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

import Week04.Vesting

test :: IO ()
test = runEmulatorTraceIO myTrace

myTrace :: EmulatorTrace ()
myTrace = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  callEndpoint @"give" h1 $ GiveParams
    { gpBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 2
    , gpDeadline    = slotToBeginPOSIXTime def 20
    , gpAmount      = 10_000_000
    }
  void $ waitUntilSlot 10
  callEndpoint @"grab" h2 ()
  s <- waitNSlots 2
  Extras.logInfo $ "reached " ++ show s

