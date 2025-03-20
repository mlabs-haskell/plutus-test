{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{- | Module: Plutus.ContextBuilder.V3.Minting
Copyright: (C) Liqwid Labs 2022
Maintainer: Koz Ross <koz@mlabs.city>
Portability: GHC only
Stability: Experimental

Builder for minting contexts. 'MintingBuilder' is an instance of 'Semigroup',
which allows combining the results of this API's functions into a larger
'MintingBuilder' using '<>'.
-}
module Plutus.ContextBuilder.V3.Minting (
  -- * Types
  MintingBuilder,

  -- * Input
  withMinting,

  -- * builder
  buildMinting',
  buildMinting,
  tryBuildMinting,
  checkMinting,
)
where

import Control.Arrow ((&&&))
import Data.Foldable (Foldable (toList))
import Data.Functor.Contravariant (contramap)
import Data.Functor.Contravariant.Divisible (choose)
import Data.Maybe (isJust)
import Optics (A_Lens, LabelOptic (labelOptic), lens, set, view)
import Plutus.ContextBuilder.V3.Base (
  BaseBuilder,
  Builder (pack, _bb),
  mintToMintValue,
  unpack,
  yieldBaseTxInfo,
  yieldExtraDatums,
  yieldInInfoDatums,
  yieldMint,
  yieldOutDatums,
  yieldRedeemer,
  yieldRedeemerMap,
 )
import Plutus.ContextBuilder.V3.Check (
  Checker,
  CheckerError,
  CheckerErrorType (OtherError),
  checkBool,
  checkFail,
  handleErrors,
  runChecker,
 )
import Plutus.ContextBuilder.V3.Internal (Normalizer (mkNormalized'), mkNormalized)
import PlutusLedgerApi.V3 (
  CurrencySymbol,
  MintValue,
  ScriptContext (ScriptContext),
  ScriptInfo (MintingScript),
  TxInfo (
    txInfoCurrentTreasuryAmount,
    txInfoData,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoProposalProcedures,
    txInfoRedeemers,
    txInfoReferenceInputs,
    txInfoSignatories,
    txInfoTreasuryDonation,
    txInfoTxCerts,
    txInfoVotes,
    txInfoWdrl
  ),
  adaSymbol,
  mintValueToMap,
 )
import PlutusTx.AssocMap qualified as AssocMap
import Prettyprinter qualified as P (Pretty (pretty))

{- | A context builder for Minting. Corresponds to
'Plutus.V1.Ledger.Contexts.Minting' specifically.

@since 3.0.0
-}
data MintingBuilder = MB BaseBuilder (Maybe CurrencySymbol)
  deriving stock
    ( -- | @since 3.0.0
      Show
    )

-- | @since 3.0.0
instance
  (k ~ A_Lens, a ~ BaseBuilder, b ~ BaseBuilder) =>
  LabelOptic "inner" k MintingBuilder MintingBuilder a b
  where
  labelOptic = lens (\(MB x _) -> x) $ \(MB _ cs) inner' -> MB inner' cs

-- | @since 3.0.0
instance
  (k ~ A_Lens, a ~ Maybe CurrencySymbol, b ~ Maybe CurrencySymbol) =>
  LabelOptic "mintingCS" k MintingBuilder MintingBuilder a b
  where
  labelOptic = lens (\(MB _ x) -> x) $ \(MB inner _) cs' -> MB inner cs'

-- | @since 3.0.0
instance Semigroup MintingBuilder where
  MB inner _ <> MB inner' cs@(Just _) =
    MB (inner <> inner') cs
  MB inner cs <> MB inner' Nothing =
    MB (inner <> inner') cs

-- | @since 3.0.0
instance Monoid MintingBuilder where
  mempty = MB mempty Nothing

-- | @since 3.0.0
instance Builder MintingBuilder where
  _bb = #inner
  pack x = set #inner x (mempty :: MintingBuilder)

instance Normalizer MintingBuilder where
  mkNormalized' (MB bb cs) =
    MB (mkNormalized bb) cs

{- | Set CurrencySymbol for building Minting ScriptContext.

@since 3.0.0
-}
withMinting :: CurrencySymbol -> MintingBuilder
withMinting cs = MB mempty $ Just cs

{- | Builds @ScriptContext@ according to given configuration and
@MintingBuilder@.

@since 3.0.0
-}
buildMinting' ::
  MintingBuilder ->
  ScriptContext
buildMinting' builder@(unpack -> bb) =
  let (ins, inDat) = yieldInInfoDatums . view #inputs $ bb
      (refin, _) = yieldInInfoDatums . view #referenceInputs $ bb
      (outs, outDat) = yieldOutDatums . view #outputs $ bb
      mintedValue = yieldMint . view #mints $ bb
      extraDat = yieldExtraDatums . view #datums $ bb
      base = yieldBaseTxInfo builder
      redeemerMap = yieldRedeemerMap (view #inputs bb) (view #mints bb)
      redeemers = AssocMap.unsafeFromList $ toList (view #redeemers bb) <> redeemerMap
      txinfo =
        base
          { txInfoInputs = ins
          , txInfoReferenceInputs = refin
          , txInfoOutputs = outs
          , txInfoData = AssocMap.unsafeFromList $ inDat <> outDat <> extraDat
          , txInfoMint = mintedValue
          , txInfoRedeemers = redeemers
          , txInfoSignatories = toList . view #signatures $ bb
          , txInfoWdrl = AssocMap.unsafeFromList $ toList (view #withdrawals bb)
          , txInfoTxCerts = toList (view #txcerts bb)
          , txInfoVotes = AssocMap.unsafeFromList []
          , txInfoProposalProcedures = mempty
          , txInfoCurrentTreasuryAmount = mempty
          , txInfoTreasuryDonation = mempty
          }
      mintSI = case view #mintingCS builder of
        Just cs ->
          if hasCS mintedValue cs
            then MintingScript cs
            else MintingScript adaSymbol
        Nothing -> MintingScript adaSymbol
      redeemer = yieldRedeemer . view #redeemer $ bb
   in ScriptContext txinfo redeemer mintSI

{- | Check builder with provided checker, then build minting context.

@since 3.0.0
-}
buildMinting :: [Checker MintingError MintingBuilder] -> MintingBuilder -> ScriptContext
buildMinting c = buildMinting' . handleErrors (mconcat c <> checkMinting)

{- | Same as `buildMinting` but instead of throwing error it returns `Either`.

@since 3.0.0
-}
tryBuildMinting :: Checker MintingError MintingBuilder -> MintingBuilder -> Either [CheckerError MintingError] ScriptContext
tryBuildMinting c b = case toList $ runChecker (c <> checkMinting) b of
  [] -> Right $ buildMinting' b
  errs -> Left errs

-- | @since 3.0.0
data MintingError
  = MintingCurrencySymbolNotGiven
  | MintingCurrencySymbolNotFound
  deriving stock (Show)

-- | @since 3.0.0
instance P.Pretty MintingError where
  pretty MintingCurrencySymbolNotGiven = "Minting Currency Symbol is not given"
  pretty MintingCurrencySymbolNotFound = "Specified Currency Symbol is not found on mints"

-- | @since 3.0.0
checkMinting :: Checker MintingError MintingBuilder
checkMinting =
  contramap
    ((foldMap mintToMintValue . toList . view #mints . unpack) &&& view #mintingCS)
    ( choose
        (\(mints, mayInner) -> maybe (Left ()) (Right . hasCS mints) mayInner)
        (checkFail $ OtherError MintingCurrencySymbolNotGiven)
        (checkBool $ OtherError MintingCurrencySymbolNotFound)
    )

hasCS :: MintValue -> CurrencySymbol -> Bool
hasCS val cs = isJust $ AssocMap.lookup cs $ mintValueToMap val
