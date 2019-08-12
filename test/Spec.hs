
--------------------------------------------------------------------------------

import Test.Hspec

import Language.MSH
import qualified Counter as C
import qualified Expr as E
import qualified Expr2 as E2
import qualified List as L
import qualified Stack as S

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "Counter" $ do 
        describe "Counter" $ do 
            it "c.!next is 0" $ result (C.c.!C.next) `shouldBe` 0
            it "(object (c.!next)).!next is 1" $ result ((object (C.c.!C.next)).!C.next) `shouldBe` 1
        describe "NameGen" $ do 
            it "ng.!newName is \"var0\"" $ result (C.ng.!C.newName) `shouldBe` "var0"
    describe "Expr" $ do
        it "v is 5" $ result (E.v.!E.eval) `shouldBe` 5
        it "e is 5" $ result (E.e.!E.eval) `shouldBe` 5
        it "a is 10" $ result (E.a.!E.eval) `shouldBe` 10
        it "bar is 20" $ E.bar `shouldBe` 20
    describe "Expr2" $ do
        it "v is 5" $ result (E2.v.!E2.eval) `shouldBe` 5
        it "e is 5" $ result (E2.e.!E2.eval) `shouldBe` 5
        it "a is 10" $ result (E2.a.!E2.eval) `shouldBe` 10
        it "b is 10" $ result (E2.b.!E2.eval) `shouldBe` 10
        it "bar is 30" $ E2.bar `shouldBe` 30
    describe "List" $ do
        it "should build lists" $ L.test (new Nothing) `shouldBe` [23,16,42,24]
        it "should sort lists" $ L.test (upcast (new (Nothing,(>)) :: L.SList Int)) `shouldBe` [16,23,24,42]
    describe "Stack" $ do
        it "pattern synonyms are working" $
            (S.emptyStack :: S.Stack Int) `shouldSatisfy` S.isEmptyStack
