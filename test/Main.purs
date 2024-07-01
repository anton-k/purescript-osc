module Test.Main where

import Prelude
import Network.Osc
  ( OscValue (..), getAddressPathArgs
  , toOscCase
  , oscCase
  )
import Effect (Effect)
import Test.Spec.Runner (runSpec)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Effect.Aff (launchAff_)
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))

main :: Effect Unit
main = do
  launchAff_ $ runSpec [ consoleReporter ] do
    testCapturePath
    testOscCase
  where
    testCapturePath =
      describe "parse OSC message from path" do
        it "address match" do
          getAddressPathArgs "/foo/bar" "/foo/bar" `shouldEqual` (Just [])
        it "address mismatch" do
          getAddressPathArgs "/foo/bar" "/foo/qux" `shouldEqual` Nothing
        it "Extract args" do
          getAddressPathArgs "/foo/$f/$s/flag/$b" "/foo/12/joe/flag/true"
            `shouldEqual` (Just [OscDouble 12.0, OscString "joe", OscBoolean true])

    testOscCase =
      describe "OSC case, parsing inputs from incoming OSC messages" do
        let
          caseExpr =
            [ toOscCase "/foo/bar" (const "foo" :: Unit -> String)
            , toOscCase "/add/$i/$i" ((\(Tuple a b) -> show (a + b)) :: Tuple Int Int -> String)
            , toOscCase "/add/arg/$i" ((\(Tuple a b) -> show (a + b)) :: Tuple Int Int -> String)
            , toOscCase "/baz" (const "baz" :: Unit -> String)
            , toOscCase "/not" ((\b -> show (not b)) :: Boolean -> String)
            ]
        it "simple case" do
          oscCase
            {address: "/foo/bar", args: []}
            caseExpr `shouldEqual` Just "foo"
        it "simple case" do
          oscCase
            {address: "/baz", args: []}
            caseExpr `shouldEqual` Just "baz"
        it "add case" do
          oscCase
            {address: "/add/2/2", args: []}
            caseExpr `shouldEqual` Just "4"
        it "add arg" do
          oscCase
            {address: "/add/arg/2", args: [OscInt 3]}
            caseExpr `shouldEqual` Just "5"
        it "add arg: not enough arguments" do
          oscCase
            {address: "/add/arg/2", args: []}
            caseExpr `shouldEqual` Nothing
        it "not arg" do
          oscCase
            {address: "/not", args: [OscBoolean true]}
            caseExpr `shouldEqual` (Just "false")

