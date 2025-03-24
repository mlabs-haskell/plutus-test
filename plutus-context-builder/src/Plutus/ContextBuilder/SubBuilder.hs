{-# LANGUAGE ViewPatterns #-}

module Plutus.ContextBuilder.SubBuilder (
  SubBuilder (..),
  buildTxOut,
  buildTxInInfo,
  buildTxOuts,
  buildTxInInfos,
  buildDatumHashPairs,
) where

import Data.Foldable (Foldable (toList))
import Data.Maybe (fromMaybe, mapMaybe)
import Optics (lens, view)
import Plutus.ContextBuilder.Base (
  BaseBuilder,
  Builder (..),
  UTXO,
  datumWithHash,
  unpack,
  utxoDatumPair,
  utxoToTxOut,
  yieldInInfoDatums,
 )
import Plutus.ContextBuilder.Internal (Normalizer (mkNormalized'), mkNormalized)
import PlutusLedgerApi.V3 (
  Datum,
  DatumHash,
  TxInInfo (TxInInfo),
  TxOut,
  TxOutRef (TxOutRef),
 )

{- | Smaller builder that builds context smaller than TxInfo.

 @since WIP
-}
newtype SubBuilder
  = SubBuilder BaseBuilder
  deriving
    ( -- | @since WIP
      Semigroup
    , -- | @since WIP
      Monoid
    )
    via BaseBuilder

-- | @since WIP
instance Builder SubBuilder where
  _bb = lens (\(SubBuilder x) -> x) (\_ b -> SubBuilder b)
  pack = SubBuilder

-- | @since WIP
instance Normalizer SubBuilder where
  mkNormalized' (SubBuilder x) = SubBuilder $ mkNormalized x

{- | Builds TxOut from `UTXO`.

 @since WIP
-}
buildTxOut :: UTXO -> TxOut
buildTxOut = utxoToTxOut

{- | Builds 'TxInInfo' from `UTXO`. If TxId or TxIdx is not set, this will use
     a default value ("" and 0, respectively) to create the 'TxInInfo'.

 @since WIP
-}
buildTxInInfo :: UTXO -> TxInInfo
buildTxInInfo u =
  let txid = fromMaybe "" $ view #txId u
      txidx = fromMaybe 0 $ view #txIdx u
   in TxInInfo (TxOutRef txid txidx) (utxoToTxOut u)

{- | Builds all TxOuts from given builder.

 @since WIP
-}
buildTxOuts :: SubBuilder -> [TxOut]
buildTxOuts (unpack -> bb) = utxoToTxOut <$> toList (view #outputs bb)

{- | Builds all TxInInfos from given builder. Returns reason when failed.

 @since WIP
-}
buildTxInInfos :: SubBuilder -> [TxInInfo]
buildTxInInfos (unpack -> bb) =
  fst $ yieldInInfoDatums (view #inputs bb)

{- | Builds Datum-Hash pair from all inputs, outputs, extra data of given builder.

 @since WIP
-}
buildDatumHashPairs :: SubBuilder -> [(DatumHash, Datum)]
buildDatumHashPairs (unpack -> bb) =
  mapMaybe utxoDatumPair (toList (view #inputs bb <> view #outputs bb))
    <> (datumWithHash <$> toList (view #datums bb))
