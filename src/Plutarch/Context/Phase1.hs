{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-all #-}

module Plutarch.Context.Phase1 (
    inOutZeroSum,
    validDatumPairs,
    positiveValues,
    signaturesProvided,
    signaturesFormat,
    validTxId,
    credentialFormat,
    phase1Check,
    customCheck,
) where

import Data.Foldable (toList)
import Data.Validation
import Plutarch.Context.Base (BaseBuilder (..), Builder, UTXO (..), unpack)
import PlutusLedgerApi.V1
import PlutusLedgerApi.V1.Value
import PlutusTx.Builtins

type P1Validator = forall a. Builder a => a -> Validation [String] a

{- | Checks if inputs, outputs, and minting is correct and zero sum.

 @since 1.2.0
-}
inOutZeroSum :: P1Validator
inOutZeroSum b@(unpack -> BB{..})
    | i <> m <> bbFee == o = Success b
    | otherwise = Failure ["Inputs, Outputs, and Mints are not zero sum"]
  where
    i = mconcat . toList $ utxoValue <$> bbInputs
    o = mconcat . toList $ utxoValue <$> bbOutputs
    m = mconcat . toList $ bbMints

{- | Checks if datum pairs are correct. We don't have to check actual
   TxOuts as their datum pairs are generated and thus is always correct.

 @since 1.2.0
-}
validDatumPairs :: P1Validator
validDatumPairs b@(unpack -> BB{..})
    | null bbDatums = Success b
    | otherwise = Failure ["Extra datum cannot be provided"]

{- | Checks if all values from input and outputs is positive.

@since 1.2.0
-}
positiveValues :: P1Validator
positiveValues b@(unpack -> BB{..})
    | f bbInputs && f bbOutputs = Success b
    | otherwise = Failure ["Inputs and outputs cannot be negative or zero"]
  where
    isPos = all (\(_, _, x) -> x > 0) . flattenValue
    f = and . fmap (isPos . utxoValue)

{- | Checks if at least one signature is provided.

 @since 1.2.0
-}
signaturesProvided :: P1Validator
signaturesProvided b@(unpack -> BB{..})
    | not $ null bbSignatures = Success b
    | otherwise = Failure ["Need at least one signature"]

{- | Checks if signatures are valid: 28 bytes long

 @since 1.2.0
-}
signaturesFormat :: P1Validator
signaturesFormat b@(unpack -> BB{..})
    | c = Success b
    | otherwise = Failure ["Signatures needs to be 28 bytes long"]
  where
    c = all (\(PubKeyHash x) -> lengthOfByteString x == 28) bbSignatures

{- | Checks if TxId is valid: 28 bytes long

 @since 1.2.0
-}
validTxId :: P1Validator
validTxId b@(unpack -> BB{..})
    | lengthOfByteString (getTxId bbTxId) == 28 = Success b
    | otherwise = Failure ["TxId needs to be 28 bytes long"]

{- | Checks if all credentials are valid: 28 bytes long

 @since 1.2.0
-}
credentialFormat :: P1Validator
credentialFormat b@(unpack -> BB{..})
    | f bbInputs && f bbOutputs = Success b
    | otherwise = Failure ["Credential must be 28 bytes long"]
  where
    bs (PubKeyCredential (PubKeyHash pkh)) = pkh
    bs (ScriptCredential (ValidatorHash vh)) = vh
    f = all (\x -> lengthOfByteString (bs x) == 28) . fmap utxoCredential

{- | Check all phase-1 validation.

 @since 1.2.0
-}
phase1Check :: P1Validator
phase1Check b =
    customCheck
        [ inOutZeroSum
        , validDatumPairs
        , positiveValues
        , signaturesProvided
        , signaturesFormat
        , validTxId
        , credentialFormat
        ]
        b

{- | Make custom validation with provided validation components.

 @since 1.2.0
-}
customCheck ::
    [(a -> Validation [String] a)] ->
    a ->
    Validation [String] a
customCheck cks x = foldMap ($ x) cks
