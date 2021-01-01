import Test.Hspec
import Parser

specParse :: Spec
specParse = do
  describe "test" $ do
    it "test0" $
      show (parse "xy") `shouldBe` "xy"
    it "test1" $
      show (parse "((xy)z)") `shouldBe` "xyz"

main :: IO ()
main = hspec $ do
  specParse

