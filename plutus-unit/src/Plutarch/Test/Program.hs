{-# LANGUAGE TemplateHaskell #-}

{- | Module: Plutarch.Test.Program
 Copyright: (C) Liqwid Labs 2022
 Copyright: (C) MLabs 2025
 Maintainer: Tomasz Maciosowski <tomasz@mlabs.city>

 Tasty provider for testing 'ScriptCase'.

@since 2.0
-}
module Plutarch.Test.Program (
  -- * High level
  ScriptCase (..),
  ScriptResult (..),
  testScript,
  testScriptGroup,

  -- * Low level functions
  runScript,
) where

import GHC.Generics (Generic)

--------------------------------------------------------------------------------

import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Data.Text qualified as T
import Optics.Core (view)
import Optics.TH (makeFieldLabelsNoPrefix)
import Plutarch.Test.Eval (evalScript)
import Test.Tasty.Providers (
  IsTest (
    run,
    testOptions
  ),
  TestTree,
  singleTest,
  testFailed,
  testPassed,
 )
import UntypedPlutusCore qualified as UPLC

--------------------------------------------------------------------------------

import Test.Tasty (testGroup)

--------------------------------------------------------------------------------

-- | @since 2.0
data ScriptResult
  = -- | @since 2.0
    ScriptSuccess
  | -- | @since 2.0
    ScriptFailure
  deriving stock
    ( -- | @since 2.0
      Eq
    , -- | @since 2.0
      Show
    )

{- | Full script info for testing.

@since 2.0
-}
data ScriptCase = ScriptCase
  { name :: String
  -- ^ The name.
  --
  -- @since 2.0
  , expectation :: ScriptResult
  -- ^ The expectation.
  --
  -- @since 2.0
  , script :: UPLC.Program UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
  -- ^ The script.
  --
  -- @since 2.0
  , debugScript :: UPLC.Program UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
  -- ^ Debug version of the script for .
  --
  -- @since 2.0
  }
  deriving stock
    ( -- | @since 2.0
      Eq
    , -- | @since 2.0
      Generic
    , -- | @since 2.0
      Show
    )

-- | @since 2.0
makeFieldLabelsNoPrefix ''ScriptCase

-- | @since 2.0
instance IsTest ScriptCase where
  testOptions = Tagged []
  run _options sc _progress = do
    case (view #expectation sc, runScriptCase sc) of
      -- expected success, received failure
      (ScriptSuccess, (ScriptFailure, msg)) -> pure $ testFailed msg
      -- expected failure, received success
      (ScriptFailure, (ScriptSuccess, msg)) -> pure $ testFailed msg
      _ -> pure $ testPassed ""

{- | Turns a 'ScriptCase' into a 'TestTree' using its 'name'.

@since 2.0
-}
testScript :: ScriptCase -> TestTree
testScript sc = singleTest (view #name sc) sc

{- | 'testGroup' but for 'ScriptCase'.

@since 2.0
-}
testScriptGroup :: String -> [ScriptCase] -> TestTree
testScriptGroup desc scs = testGroup desc $ testScript <$> scs

{- | Low-level function for running a 'ScriptCase'.

@since 2.0
-}
runScriptCase :: ScriptCase -> (ScriptResult, String)
runScriptCase sc =
  runScript
    (view #script sc)
    (view #debugScript sc)
    msg
  where
    msg = case view #expectation sc of
      ScriptSuccess -> ""
      ScriptFailure -> "Expected failure, but script succeeded"

{- | Low-level function for running a script.

@since 2.0
-}
runScript ::
  -- | Script to run.
  UPLC.Program UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun () ->
  -- | Debug version of the script.
  UPLC.Program UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun () ->
  -- | Message to return upon success.
  String ->
  -- | Returns the result of evaluating the script, along with the parameter
  -- message upon success, or an error message upon failure.
  (ScriptResult, String)
runScript script debug onSuccess = case scriptResult of
  (Right _, _, _) -> (ScriptSuccess, onSuccess)
  (Left err, _, _) -> (ScriptFailure, showError dTrace (show err))
  where
    scriptResult = evalScript script
    (_, _, dTrace) = evalScript debug

showError :: [Text] -> String -> String
showError traces err =
  "Script failed with error: "
    <> err
    <> "\nTrace Log:\n"
    <> T.unpack (T.intercalate "\n" traces)
