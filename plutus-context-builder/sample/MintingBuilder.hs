module MintingBuilder (specs) where

import Plutus.ContextBuilder (
  MintingBuilder,
  mint,
  tryBuildMinting,
  withMinting,
 )
import PlutusLedgerApi.V3 (CurrencySymbol (CurrencySymbol), Redeemer (Redeemer), ToData (toBuiltinData), TokenName (TokenName), singleton)
import Prettyprinter qualified as P
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

specs :: TestTree
specs =
  testGroup
    "Minting Builder Unit Tests"
    [ testCase "MintingBuilder succeeds with single input" $
        case tryBuildMinting mempty unitRedeemer $ singleMint <> withMinting (CurrencySymbol "deadbeef") of
          Left err -> assertFailure ("buildingMinting failed with error: " <> show (P.pretty err))
          Right _ -> pure ()
    , testCase "MintingBuilder fails if currency symbol can't be found" $
        case tryBuildMinting mempty unitRedeemer $ singleMint <> withMinting (CurrencySymbol "beefbeef") of
          Left _ -> pure ()
          Right _ ->
            assertFailure
              ( "tryBuildMinting mempty should fail invalid CS is"
                  <> " passed, but it succeeded."
              )
    , testCase "MintingBuilder fails with unspecified currency symbol" $
        case tryBuildMinting mempty unitRedeemer mempty of
          Left _ -> pure ()
          Right _ ->
            assertFailure
              ( "tryBuildMinting mempty should fail when mbMintingCS,"
                  <> " but it passed."
              )
    , testCase "MintingBuilder works with either of two Minting CS's" $
        case tryBuildMinting mempty unitRedeemer $ doubleMint <> withMinting (CurrencySymbol "deadbeef") of
          Left err -> assertFailure ("tryBuildMinting mempty failed with error " <> show (P.pretty err))
          Right _ -> case tryBuildMinting mempty unitRedeemer $ doubleMint <> withMinting (CurrencySymbol "bebe") of
            Left err -> assertFailure ("tryBuildMinting mempty failed with error " <> show (P.pretty err))
            Right _ -> pure ()
    ]

singleMint :: MintingBuilder
singleMint = mint (singleton (CurrencySymbol "deadbeef") (TokenName "alivecow") 1)

doubleMint :: MintingBuilder
doubleMint = singleMint <> mint (singleton (CurrencySymbol "bebe") (TokenName "smallcow") 1)

unitRedeemer :: Redeemer
unitRedeemer = Redeemer $ toBuiltinData ()
