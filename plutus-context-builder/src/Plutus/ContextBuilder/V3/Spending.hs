{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{- | Module: Plutus.ContextBuilder.V3.Spending
Copyright: (C) Liqwid Labs 2022
Maintainer: Koz Ross <koz@mlabs.city>
Portability: GHC only
Stability: Experimental

Builder for spending contexts. 'SpendingBuilder' is an instance of 'Semigroup',
which allows combining the results of this API's functions into a larger
'SpendingBuilder' using '<>'.
-}
module Plutus.ContextBuilder.V3.Spending (
  -- * Types
  SpendingBuilder,

  -- * Inputs
  withSpendingUTXO,
  withSpendingOutRef,
  withSpendingOutRefId,
  withSpendingOutRefIdx,
  withDatum,

  -- * builder
  buildSpending',
  buildSpending,
  tryBuildSpending,
  checkSpending,
)
where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Data.Foldable (Foldable (toList))
import Data.Functor.Contravariant (contramap)
import Data.Functor.Contravariant.Divisible (choose)
import Data.Maybe (isJust)
import Optics (A_Lens, LabelOptic (labelOptic), lens, set, view)
import Plutus.ContextBuilder.V3.Base (
  BaseBuilder,
  Builder (pack, _bb),
  UTXO,
  normalizeUTXO,
  unpack,
  utxoToTxOut,
  yieldBaseTxInfo,
  yieldExtraDatums,
  yieldInInfoDatums,
  yieldMint,
  yieldOutDatums,
  yieldRedeemer,
  yieldRedeemerMap,
 )
import Plutus.ContextBuilder.V3.Check
import Plutus.ContextBuilder.V3.Internal (Normalizer (mkNormalized'), mkNormalized)
import PlutusLedgerApi.V3 (
  Datum,
  ScriptContext (ScriptContext),
  ScriptInfo (SpendingScript),
  TxId,
  TxInInfo (txInInfoOutRef, txInInfoResolved),
  TxInfo (
    txInfoData,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoRedeemers,
    txInfoReferenceInputs,
    txInfoSignatories,
    txInfoTxCerts,
    txInfoWdrl
  ),
  TxOutRef (..),
 )
import PlutusTx.AssocMap qualified as AssocMap
import Prettyprinter qualified as P

data ValidatorInputIdentifier
  = ValidatorUTXO UTXO
  | ValidatorOutRef TxOutRef
  | ValidatorOutRefId TxId
  | ValidatorOutRefIdx Integer
  deriving stock (Show)

{- | A context builder for spending. Corresponds broadly to validators, and to
'PlutusLedgerApi.V1.Contexts.Spending' specifically.

@since 3.0.0
-}
data SpendingBuilder = SB BaseBuilder (Maybe ValidatorInputIdentifier) (Maybe Datum)

{-
  { sbInner :: BaseBuilder
  , sbValidatorInput :: Maybe ValidatorInputIdentifier
  }
  -}

-- | @since 3.0.0
instance
  (k ~ A_Lens, a ~ BaseBuilder, b ~ BaseBuilder) =>
  LabelOptic "inner" k SpendingBuilder SpendingBuilder a b
  where
  labelOptic = lens (\(SB x _ _) -> x) $ \(SB _ vi dat) inner' -> SB inner' vi dat

-- | @since 3.0.0
instance
  (k ~ A_Lens, a ~ Maybe ValidatorInputIdentifier, b ~ Maybe ValidatorInputIdentifier) =>
  LabelOptic "validatorInput" k SpendingBuilder SpendingBuilder a b
  where
  labelOptic = lens (\(SB _ x _) -> x) $ \(SB inner _ dat) vi' -> SB inner vi' dat

-- | @since 3.0.0
instance
  (k ~ A_Lens, a ~ Maybe Datum, b ~ Maybe Datum) =>
  LabelOptic "datum" k SpendingBuilder SpendingBuilder a b
  where
  labelOptic = lens (\(SB _ _ x) -> x) $ \(SB inner vi _) dat' -> SB inner vi dat'

-- | @since 3.0.0
instance Builder SpendingBuilder where
  _bb = #inner
  pack x = set #inner x (mempty :: SpendingBuilder)

-- | @since 3.0.0
instance Semigroup SpendingBuilder where
  SB inner vin dat <> SB inner' vin' dat' = SB (inner <> inner') (vin' <|> vin) (dat' <|> dat)

-- | @since 3.0.0
instance Monoid SpendingBuilder where
  mempty = SB mempty Nothing Nothing

instance Normalizer SpendingBuilder where
  mkNormalized' (SB bb vi dat) =
    SB (mkNormalized bb) (normalize <$> vi) dat
    where
      normalize x =
        case x of
          ValidatorUTXO utxo -> ValidatorUTXO $ normalizeUTXO utxo
          a -> a

{- | Set Validator Input with given UTXO. Note, the given UTXO should
exist in the inputs, otherwise the builder would fail.

@since 3.0.0
-}
withSpendingUTXO ::
  UTXO ->
  SpendingBuilder
withSpendingUTXO u =
  set #validatorInput (pure . ValidatorUTXO $ u) (mempty :: SpendingBuilder)

{- | Set Validator Input with given TxOutRef. Note, input with given
TxOutRef should exist, otherwise the builder would fail.

@since 3.0.0
-}
withSpendingOutRef ::
  TxOutRef ->
  SpendingBuilder
withSpendingOutRef outref =
  set #validatorInput (pure . ValidatorOutRef $ outref) (mempty :: SpendingBuilder)

{- | Set Validator Input with given TxOutRefId. Note, input with given
TxOutRefId should exist, otherwise the builder would fail.

@since 3.0.0
-}
withSpendingOutRefId ::
  TxId ->
  SpendingBuilder
withSpendingOutRefId tid =
  set #validatorInput (pure . ValidatorOutRefId $ tid) (mempty :: SpendingBuilder)

{- | Set Validator Input with given TxOutRefIdx. Note, input with given
TxOutRefIdx should exist, otherwise the builder would fail.

@since 3.0.0
-}
withSpendingOutRefIdx ::
  Integer ->
  SpendingBuilder
withSpendingOutRefIdx tidx =
  set #validatorInput (pure . ValidatorOutRefIdx $ tidx) (mempty :: SpendingBuilder)

{- | Set Validator Input with given Datum.

@since 3.0.0
-}
withDatum ::
  Datum ->
  SpendingBuilder
withDatum datum =
  set #datum (Just datum) (mempty :: SpendingBuilder)

yieldValidatorInput ::
  [TxInInfo] ->
  ValidatorInputIdentifier ->
  Maybe TxOutRef
yieldValidatorInput ins = \case
  ValidatorUTXO utxo -> go txInInfoResolved (utxoToTxOut utxo)
  ValidatorOutRef outref -> go txInInfoOutRef outref
  ValidatorOutRefId tid -> go (txOutRefId . txInInfoOutRef) tid
  ValidatorOutRefIdx tidx -> go (txOutRefIdx . txInInfoOutRef) tidx
  where
    go :: (Eq b) => (TxInInfo -> b) -> b -> Maybe TxOutRef
    go f x =
      case filter (\(f -> y) -> y == x) ins of
        [] -> Nothing
        (r : _) -> return $ txInInfoOutRef r

{- | Builds @ScriptContext@ according to given configuration and
@SpendingBuilder@.

@since 3.0.0
-}
buildSpending' ::
  SpendingBuilder ->
  ScriptContext
buildSpending' builder@(unpack -> bb) =
  let (ins, inDat) = yieldInInfoDatums . view #inputs $ bb
      (refin, _) = yieldInInfoDatums . view #referenceInputs $ bb
      (outs, outDat) = yieldOutDatums . view #outputs $ bb
      mintedValue = yieldMint . view #mints $ bb
      extraDat = yieldExtraDatums . view #datums $ bb
      base = yieldBaseTxInfo builder
      redeemerMap = yieldRedeemerMap (view #inputs bb) (view #mints bb)
      txinfo =
        base
          { txInfoInputs = ins
          , txInfoReferenceInputs = refin
          , txInfoOutputs = outs
          , txInfoData = AssocMap.unsafeFromList $ inDat <> outDat <> extraDat
          , txInfoMint = mintedValue
          , txInfoSignatories = toList . view #signatures $ bb
          , txInfoRedeemers = AssocMap.unsafeFromList $ toList (view #redeemers bb) <> redeemerMap
          , txInfoWdrl = AssocMap.unsafeFromList $ toList (view #withdrawals bb)
          , txInfoTxCerts = toList (view #txcerts bb)
          }
      vInRef = case view #validatorInput builder >>= yieldValidatorInput ins of
        Nothing -> TxOutRef "" 0
        Just ref -> ref
      datum = view #datum builder
      redeemer = yieldRedeemer . view #redeemer $ bb
   in ScriptContext txinfo redeemer (SpendingScript vInRef datum)

{- | Check builder with provided checker, then build spending context.

@since 3.0.0
-}
buildSpending :: [Checker SpendingError SpendingBuilder] -> SpendingBuilder -> ScriptContext
buildSpending c = buildSpending' . handleErrors (mconcat c <> checkSpending)

{- | Same as `buildSpending` but instead of throwing error it returns `Either`.

@since 3.0.0
-}
tryBuildSpending :: Checker SpendingError SpendingBuilder -> SpendingBuilder -> Either [CheckerError SpendingError] ScriptContext
tryBuildSpending c b = case toList $ runChecker (c <> checkSpending) b of
  [] -> Right $ buildSpending' b
  errs -> Left errs

-- | @since 3.0.0
data SpendingError
  = ValidatorInputDoesNotExists ValidatorInputIdentifier
  | ValidatorInputNotGiven
  deriving stock (Show)

-- | @since 3.0.0
instance P.Pretty SpendingError where
  pretty (ValidatorInputDoesNotExists x) =
    "Given validator input does not exist in inputs: "
      <> P.line
      <> P.indent 4 (P.pretty (show x))
  pretty ValidatorInputNotGiven = "Validator Input is not specified"

-- | @since 3.0.0
checkSpending :: Checker SpendingError SpendingBuilder
checkSpending =
  checkAt AtInput $
    contramap
      ((fst . yieldInInfoDatums . view #inputs . unpack) &&& view #validatorInput)
      ( choose
          (\(ins, vin) -> maybe (Left ()) (\x -> Right (x, yieldValidatorInput ins x)) vin)
          (checkFail $ OtherError ValidatorInputNotGiven)
          ( checkWith $ \(vin, _) ->
              checkIf (isJust . snd) $ OtherError $ ValidatorInputDoesNotExists vin
          )
      )
