import Test.Hspec
import Parser
import LambdaTerm

specParse :: Spec
specParse = do
  describe "Parser.parse" $ do
    it "\"x\" to Var \"x\"" $
      parse "x" `shouldBe` Var "x"
    it "\"(xy)\" to App (Var \"x\") (Var \"y\")" $
      parse "xy" `shouldBe` App (Var "x") (Var "y")
    it "\"(\\x.y)\" to Lmd \"x\" \"y\"" $
      parse "(\\x.y)" `shouldBe` Lmd "x" (Var "y")

main :: IO ()
main = hspec $ do
  specParse

