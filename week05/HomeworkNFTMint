{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.Homework2 where

import           Control.Monad          hiding (fmap)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Semigroup (..), Show (..), String, undefined)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

{-# INLINABLE mkPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkPolicy oref () ctx =  traceIfFalse "UTXO Not Consumed" hasUTXO &&
                        traceIfFalse "Wrong Amount Minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTXO :: Bool
    hasUTXO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, _, amt)]   -> amt == 1
        _               -> False
    
    {-}
    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == "" && amt == 1  -- This creates a huge crazy Error
        _               -> False
    -}
    {-
    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, _, amt)]   -> amt == 1
        _               -> False 
    -}
    --True -- FIX ME!

policy :: TxOutRef -> Scripts.MintingPolicy
policy oref = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' -> Scripts.wrapMintingPolicy $ mkPolicy oref' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref 

{-}
policy :: TxOutRef -> Scripts.MintingPolicy
policy oref = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy $ mkPolicy oref||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref 
-}    
    -- undefined -- IMPLEMENT ME!

curSymbol :: TxOutRef -> CurrencySymbol
curSymbol oref = scriptCurrencySymbol $ policy oref 
   
    -- undefined -- IMPLEMENT ME!

data NFTParams = NFTParams
    { npToken   :: !TokenName
    , npAddress :: !Address 
    } deriving (Generic, FromJSON, ToJSON, Show)

type NFTSchema = Endpoint "mint" NFTParams

--type NFTSchema = Endpoint "mint" Address

mint :: Address -> Contract w NFTSchema Text ()
mint np = do 
    utxos <- utxosAt $ npAddress np
    case Map.keys utxos of 
        []      -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let tn      = npToken np
            let val     = Value.singleton (curSymbol oref tn) tn 1
                lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref 
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx  
            Contract.logInfo @String $ printf "forged %s" (show val)    

    --undefined -- IMPLEMENT ME!

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = ""
        w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"mint" h1 $ NFTParams
        { npToken   = tn
        , npAddress = mockWalletAddress w1
        }
    callEndpoint @"mint" h2 $ NFTParams
        { npToken   = tn
        , npAddress = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1

{-
test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = ""
        w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"mint" h1 $ mockWalletAddress w1
        { npToken   = tn
        , npAddress = mockWalletAddress w1
        }
    callEndpoint @"mint" h2 $ mockWalletAddress w2
        { npToken   = tn
        , npAddress = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1
-}


-- RResults in the Repl

{-

Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58: 
    {, ""}: 99996914
    {356e0f6179609862ab59a90e879c631491e45c5c4d06e9b3fa982c7e, ""}: 1
Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491: 
    {, ""}: 99996914
    {d0e21f3d6aff34abb9c4bc0d614b9bf2d852267a8384e5a74a69c98f, ""}: 1

-}
