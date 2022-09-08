module ExampleSpec where
import Test.Hspec
import Example

-- main is optional
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "add" $ do
        it "adds Nums" $ do
            (add 1 1) `shouldBe` (2 :: Integer)
