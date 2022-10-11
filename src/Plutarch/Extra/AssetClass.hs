{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Provides Data and Scott encoded  asset class types and utility
  functions.
-}
module Plutarch.Extra.AssetClass (
  -- * AssetClass - Hask
  AssetClass (AssetClass, symbol, name),
  assetClassValue,

  -- * AssetClass - Plutarch
  PAssetClass (PAssetClass, psymbol, pname),
  passetClass,
  passetClassT,
  adaSymbolData,
  isAdaClass,
  adaClass,
  padaClass,
  emptyTokenNameData,
  psymbolAssetClass,
  psymbolAssetClassT,
  pconstantClass,
  pconstantClassT,

  -- * AssetClassData - Plutarch
  PAssetClassData (PAssetClassData),
  passetClassData,
  passetClassDataT,

  -- * Scott <-> Data conversions
  ptoScottEncoding,
  pfromScottEncoding,
  pviaScottEncoding,
  symbolT,
  nameT,
) where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Tagged (Tagged (Tagged, unTagged), untag)
import GHC.TypeLits (Symbol)
import qualified Generics.SOP as SOP
import Optics.Internal.Optic
import Optics.Lens (Lens')
import Optics.TH (makeFieldLabelsNoPrefix)
import Plutarch.Api.V1 (
  PCurrencySymbol,
  PTokenName,
 )
import Plutarch.DataRepr (PDataFields)
import Plutarch.Extra.Applicative (ppure)
import Plutarch.Extra.IsData (
  DerivePConstantViaDataList (DerivePConstantViaDataList),
  ProductIsData (ProductIsData),
 )
import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))
import Plutarch.Extra.Tagged (PTagged)
import Plutarch.Lift (
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Orphans ()
import PlutusLedgerApi.V1.Value (CurrencySymbol, TokenName)
import qualified PlutusLedgerApi.V1.Value as Value
import qualified PlutusTx

--------------------------------------------------------------------------------
-- AssetClass & Variants

{- | A version of 'PlutusTx.AssetClass', with a tag for currency.

 @since 3.10.0
-}
data AssetClass = AssetClass
  { symbol :: CurrencySymbol
  -- ^ @since 3.9.0
  , name :: TokenName
  -- ^ @since 3.9.0
  }
  deriving stock
    ( -- | @since 3.9.0
      Eq
    , -- | @since 3.9.0
      Ord
    , -- | @since 3.9.0
      Show
    , -- | @since 3.9.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.9.0
      ToJSON
    , -- | @since 3.9.0
      FromJSON
    , -- | @since 3.9.0
      FromJSONKey
    , -- | @since 3.9.0
      ToJSONKey
    , -- | @since 3.10.0
      SOP.Generic
    )
  deriving
    ( -- | @since 3.10.0
      PlutusTx.ToData
    , -- | @since 3.10.0
      PlutusTx.FromData
    )
    via Plutarch.Extra.IsData.ProductIsData AssetClass
  deriving
    ( -- | @since 3.10.0
      Plutarch.Lift.PConstantDecl
    )
    via ( Plutarch.Extra.IsData.DerivePConstantViaDataList
            AssetClass
            PAssetClassData
        )

{- | A Scott-encoded Plutarch equivalent to 'AssetClass'.

 @since 3.10.0
-}
data PAssetClass (s :: S) = PAssetClass
  { psymbol :: Term s (PAsData PCurrencySymbol)
  -- ^ @since 3.9.0
  , pname :: Term s (PAsData PTokenName)
  -- ^ @since 3.9.0
  }
  deriving stock
    ( -- | @since 3.9.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.9.0
      PEq
    , -- | @since 3.9.0
      PlutusType
    )

-- | @since 3.10.0
instance DerivePlutusType PAssetClass where
  type DPTStrat _ = PlutusTypeScott

-- | @since 3.10.0
passetClass ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PAssetClass)
passetClass = phoistAcyclic $
  plam $ \sym tk ->
    pcon $ PAssetClass (pdata sym) (pdata tk)

-- | @since 3.10.0
passetClassT ::
  forall (unit :: Symbol) (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PTagged unit PAssetClass)
passetClassT = phoistAcyclic $
  plam $ \sym tk ->
    ppure #$ passetClass # sym # tk

-- | @since 3.10.0
pconstantClass ::
  forall (s :: S).
  AssetClass ->
  Term s PAssetClass
pconstantClass (AssetClass sym tk) =
  pcon $
    PAssetClass (pconstantData sym) (pconstantData tk)

-- | @since 3.10.0
pconstantClassT ::
  forall (unit :: Symbol) (s :: S).
  Tagged unit AssetClass ->
  Term s (PTagged unit PAssetClass)
pconstantClassT (Tagged (AssetClass sym tk)) =
  ppure #$ pcon $ PAssetClass (pconstantData sym) (pconstantData tk)

{- | Construct a 'PAssetClass' with empty 'pname'.
 @since 3.9.0
-}
psymbolAssetClass ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PAssetClass)
psymbolAssetClass = phoistAcyclic $
  plam $ \sym ->
    pcon $ PAssetClass (pdata sym) emptyTokenNameData

{- | Tagged version of `psymbolAssetClass`
 @since 3.10.0
-}
psymbolAssetClassT ::
  forall (unit :: Symbol) (s :: S).
  Term s (PCurrencySymbol :--> PTagged unit PAssetClass)
psymbolAssetClassT = phoistAcyclic $
  plam $ \sym ->
    ppure #$ pcon $ PAssetClass (pdata sym) emptyTokenNameData

--------------------------------------------------------------------------------
-- Simple Helpers

{- | Check whether an 'AssetClass' corresponds to Ada.

 @since 3.9.0
-}
isAdaClass :: AssetClass -> Bool
isAdaClass (AssetClass s n) = s == s' && n == n'
  where
    (Tagged (AssetClass s' n')) = adaClass

{- | The 'PCurrencySymbol' for Ada, corresponding to an empty string.

 @since 3.9.0
-}
adaSymbolData :: forall (s :: S). Term s (PAsData PCurrencySymbol)
adaSymbolData = pconstantData ""

{- | The 'AssetClass' for Ada, corresponding to an empty currency symbol and
 token name.

 @since 3.9.0
-}
adaClass :: Tagged "Ada" AssetClass
adaClass = Tagged $ AssetClass "" ""

{- | Plutarch equivalent for 'adaClass'.

 @since 3.9.0
-}
padaClass :: forall (s :: S). Term s (PTagged "Ada" PAssetClass)
padaClass = pconstantClassT adaClass

{- | The empty 'PTokenName'

 @since 3.9.0
-}
emptyTokenNameData :: forall (s :: S). Term s (PAsData PTokenName)
emptyTokenNameData = pconstantData ""

----------------------------------------
-- Data-Encoded version

{- | A 'PlutusTx.Data'-encoded version of 'AssetClass', without the currency
 tag.

 @since 3.10.0
-}
newtype PAssetClassData (s :: S)
  = PAssetClassData
      ( Term
          s
          ( PDataRecord
              '[ "symbol" ':= PCurrencySymbol
               , "name" ':= PTokenName
               ]
          )
      )
  deriving stock
    ( -- | @since 3.9.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.9.0
      PlutusType
    , -- | @since 3.9.0
      PEq
    , -- | @since 3.9.0
      PIsData
    , -- | @since 3.9.0
      PDataFields
    , -- | @since 3.9.0
      PShow
    )

-- | @since 3.10.0
instance DerivePlutusType PAssetClassData where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 3.10.0
instance Plutarch.Lift.PUnsafeLiftDecl PAssetClassData where
  type PLifted PAssetClassData = AssetClass

-- | @since 3.10.0
passetClassData ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PAssetClassData)
passetClassData = phoistAcyclic $
  plam $ \sym tk ->
    mkRecordConstr
      PAssetClassData
      ( #symbol .= pdata sym
          .& #name .= pdata tk
      )

-- | @since 3.10.0
passetClassDataT ::
  forall (unit :: Symbol) (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PTagged unit PAssetClassData)
passetClassDataT = phoistAcyclic $
  plam $ \sym tk ->
    ppure #$ passetClassData # sym # tk

{- | Convert from 'PAssetClassData' to 'PAssetClass'.

 @since 3.9.0
-}
ptoScottEncoding ::
  forall (s :: S).
  Term s (PAsData PAssetClassData :--> PAssetClass)
ptoScottEncoding = phoistAcyclic $
  plam $ \cls ->
    pletFields @["symbol", "name"] cls $
      \cls' ->
        pcon $
          PAssetClass
            (getField @"symbol" cls')
            (getField @"name" cls')

{- | Convert from 'PAssetClass' to 'PAssetClassData'.

 @since 3.9.0
-}
pfromScottEncoding ::
  forall (s :: S).
  Term s (PAssetClass :--> PAsData PAssetClassData)
pfromScottEncoding = phoistAcyclic $
  plam $ \cls -> pmatch cls $
    \(PAssetClass sym tk) ->
      pdata $
        mkRecordConstr
          PAssetClassData
          ( #symbol .= sym
              .& #name .= tk
          )

{- | Wrap a function using the Scott-encoded 'PAssetClass' to one using the
 'PlutusTx.Data'-encoded version.

 @since 3.9.0
-}
pviaScottEncoding ::
  forall (a :: PType).
  ClosedTerm (PAssetClass :--> a) ->
  ClosedTerm (PAsData PAssetClassData :--> a)
pviaScottEncoding fn = phoistAcyclic $
  plam $ \cls ->
    fn #$ ptoScottEncoding # cls

{- | Version of 'assetClassValue' for tagged 'AssetClass' and 'Tagged'.

 @since 3.9.0
-}
assetClassValue ::
  forall (unit :: Symbol).
  Tagged unit AssetClass ->
  Tagged unit Integer ->
  Value.Value
assetClassValue (Tagged (AssetClass sym tk)) q = Value.singleton sym tk $ untag q

----------------------------------------
-- Field Labels

-- | @since 3.9.0
makeFieldLabelsNoPrefix ''PAssetClass

-- | @since 3.9.0
makeFieldLabelsNoPrefix ''AssetClass

-- | @since 3.10.1
symbolT ::
  forall (unit :: Symbol).
  Lens' (Tagged unit AssetClass) CurrencySymbol
symbolT = #unTagged %% #symbol

-- | @since 3.10.1
nameT ::
  forall (unit :: Symbol).
  Lens' (Tagged unit AssetClass) TokenName
nameT = #unTagged %% #name
