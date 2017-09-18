
--------------------------------------------------------------------------------

import Test.Hspec

import Language.MSH
--import qualified Counter as C
import qualified Expr as E
import qualified List as L
import qualified Stack as S

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "Counter" $ return ()
    describe "Expr" $ do
        it "v is 5" $ result (E.v.!E.eval) `shouldBe` 5
        it "e is 5" $ result (E.e.!E.eval) `shouldBe` 5
        it "a is 10" $ result (E.a.!E.eval) `shouldBe` 10
    describe "List" $ do
        it "should build lists" $ L.test (new Nothing) `shouldBe` [23,16,42,24]
        it "should sort lists" $ L.test (upcast (new (Nothing,(>)) :: L.SList Int)) `shouldBe` [16,23,24,42]
    describe "Stack" $ do
        it "pattern synonyms are working" $
            (S.emptyStack :: S.Stack Int) `shouldSatisfy` S.isEmptyStack
