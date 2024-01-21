--{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
import Test.Hspec

import qualified Test.MuCheck.Utils.Common
import qualified Test.MuCheck.Utils.Print
import qualified Test.MuCheck.Mutation
import qualified Test.MuCheck.Utils.CommonSpec
import qualified Test.MuCheck.Utils.PrintSpec
import qualified Test.MuCheck.MutationSpec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Common" Test.MuCheck.Utils.CommonSpec.spec
  describe "Print" Test.MuCheck.Utils.PrintSpec.spec
  describe "Mutation" Test.MuCheck.MutationSpec.spec
