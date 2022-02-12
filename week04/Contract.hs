{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Contract where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import Data.Void                  (Void)
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

{- WE WILL APPLY WHAT WE HAVE LEARNED TO RUN CONTRACTS IN THE REPL -}

-- Contract w s e a
-- EmulatorTrace a

myContract1 :: Contract () Empty Text ()                -- the contract part is the logic to build tx's
myContract1 = do                                        -- In this example we just throw the Error in the Repl
    void $ Contract.throwError "BOOM!"
    Contract.logInfo @String "hello from the contract"

myTrace1 :: EmulatorTrace ()                            -- Trace Emulates the Contract based on provided parameters
myTrace1 = void $ activateContractWallet (knownWallet 1) myContract1

test1 :: IO ()                                          -- test is the IO used to send that info, eg, funds for Tx
test1 = runEmulatorTraceIO myTrace1

myContract2 :: Contract () Empty Void ()                -- myContract2 handles the error and reuns Contract 1 again
myContract2 = Contract.handleError
    (\err -> Contract.logError $ "caught: " ++ unpack err)
    myContract1

myTrace2 :: EmulatorTrace ()                            
myTrace2 = void $ activateContractWallet (knownWallet 1) myContract2

test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2

type MySchema = Endpoint "foo" Int .\/ Endpoint "bar" String    -- Schema type here provides contract endpoints
                                                                -- using the Vesting contract wee would have the "give" and "grab" endpoints
myContract3 :: Contract () MySchema Text ()
myContract3 = do
    awaitPromise $ endpoint @"foo" Contract.logInfo             -- awaitPromise, waits for the endpoint and continues
    awaitPromise $ endpoint @"bar" Contract.logInfo             -- the contract will hold at that point until endpoint is received

myTrace3 :: EmulatorTrace ()                                    -- Emulator calls the Endpoints
myTrace3 = do                                                   -- using the h handle as a reference to what / where
    h <- activateContractWallet (knownWallet 1) myContract3     -- call endpoint "foo" on handle h with the Integer 42 (specified in Schema)
    callEndpoint @"foo" h 42                                    -- same for "bar" with a  String
    callEndpoint @"bar" h "Haskell"

test3 :: IO ()
test3 = runEmulatorTraceIO myTrace3                             -- Test 3 runs myTrace3

myContract4 :: Contract [Int] Empty Text ()                     -- myContract4 is about whaiting for slots for actions to happen
myContract4 = do
    void $ Contract.waitNSlots 10                               -- after 10 slots, tell [1]
    tell [1]                                                    -- after 10 slots tell [2]
    void $ Contract.waitNSlots 10                               -- tell [2] wll add to the list, so it will return [1,2]
    tell [2]
    void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()                                    -- myTrace4 loggs outputs at various periods (specified slots)
myTrace4 = do
    h <- activateContractWallet (knownWallet 1) myContract4

    void $ Emulator.waitNSlots 5                                -- After 5 slots, create a handle xs with the state of h (Wallet 1)
    xs <- observableState h                                     -- show the log xs and continue
    Extras.logInfo $ show xs

    void $ Emulator.waitNSlots 10                               -- after 10 slots, repeat the above with the handle ys
    ys <- observableState h                                     -- show the log ys
    Extras.logInfo $ show ys

    void $ Emulator.waitNSlots 10                               -- finally create another observation after another 10 slots
    zs <- observableState h                                     -- observation of h is called zs, log it and show the log
    Extras.logInfo $ show zs

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4
