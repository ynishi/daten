module DatenSpec
  ( spec
  ) where

import           Daten
import           Test.Hspec

spec :: Spec
spec = do
  describe "toFormatteds" $ do
    it "creates formattted list" $ do
      let cases =
            [ ( ((2019, 1, 1), (2019, 1, 2), "%Y-%m-%d")
              , ["2019-01-01", "2019-01-02"])
            , ( ((2019, 1, 1), (2019, 1, 3), "%Y-%m-%d")
              , ["2019-01-01", "2019-01-02", "2019-01-03"])
            , (((2019, 1, 1), (2019, 1, 2), "%Y%m%d"), ["20190101", "20190102"])
            ]
      mapM_
        (\((x1, x2, x3), expected) ->
           toFormatteds (Daten x1 x2 x3) `shouldBe` expected)
        cases
  describe "join" $ do
    it "creates joined String" $ do
      let cases =
            [ ( ("',\n'", (2019, 1, 1), (2019, 1, 2), "%Y-%m-%d")
              , "'2019-01-01',\n'2019-01-02'")
            ]
      mapM_
        (\((x1, x2, x3, x4), expected) ->
           join x1 (Daten x2 x3 x4) `shouldBe` expected)
        cases
