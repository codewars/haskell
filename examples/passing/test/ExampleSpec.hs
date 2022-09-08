module ExampleSpec where
import Test.Hspec
import Example

spec :: Spec
spec = do
  describe "add" $ do
    it "adds Nums" $ do
      (add 1 1) `shouldBe` (2 :: Integer)
