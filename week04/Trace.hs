{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Week04.Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger.TimeSlot
import Plutus.Trace
import Wallet.Emulator.Wallet

import Week04.Vesting

-- Contract w s e a
-- EmulatorTrace a

test :: IO ()
test = runEmulatorTraceIO myTrace       -- test will run the Trace and display it in the console

myTrace :: EmulatorTrace ()             -- defines a trace in the Emulator Trace monad
myTrace = do                            -- in the Playground we can start calling endpoints
    h1 <- activateContractWallet (knownWallet 1) endpoints      -- in Emulator Trace we need to activate the contract first
    h2 <- activateContractWallet (knownWallet 2) endpoints      -- so we run the activateContractWallet - it runs the wallets and the contract endpoints
    callEndpoint @"give" h1 $ GiveParams                        -- The result is a contract Handle h1 and h2 for eacch wallet
        { gpBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 2       -- In order to call an endpoint we need to use callEndpint @"give" which is a type endpoint
        , gpDeadline    = slotToBeginPOSIXTime def 20           -- this is called on h1, and it applies the GiveParams for the contract
        , gpAmount      = 10000000                              -- we gve the timframe for the deadline, and the amount in lovelace
        }                                                       
    void $ waitUntilSlot 20                                     -- now wallet 2 calls the Grab endpoint
    callEndpoint @"grab" h2 ()                                  -- we wait another 2 slots
    s <- waitNSlots 2                                           -- then we log the info
    Extras.logInfo $ "reached " ++ show s                       -- the Extras.logInfo 

    -- waitUntilSlot specifies a limit on time, waitNSlots waits for a given period
