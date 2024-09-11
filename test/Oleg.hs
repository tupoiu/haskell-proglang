module Oleg where

import Prototypes.OlegConstraintsTypechecker
import Prototypes.OlegParser
import Test.Hspec

shouldError x = shouldThrow (print x) anyErrorCall
itShouldTypecheckTo str tx = it ("should typecheck " <> str <> " to " <> show tx) (tc str == tx)
itShouldThrowTypeError str = it ("should fail typechecking at " <> str) $ shouldError $ tc str
shouldBeLeft x = case x of
  Left _ -> True
  Right _ -> False

typechecking = describe "Typechecking" $
  do
    it "should say that TV 0 occurs in TV 0" $ tvOccurs tvenv0 0 (TV 0) == True
    it "should say that TV 0 occurs in TV 0 :> TV 0" $ tvOccurs tvenv0 0 (TV 0 :> TV 0) == True
    it "should say that TV 0 doesn't occur in TV 1 :> TV 1" $ tvOccurs tvenv0 0 (TV 1 :> TV 1) == False
    it "should say that TV 0 occurs in TV 0 :> TV 0" $ tvOccurs tvenv0 0 (TV 1 :> TV 0) == True
    it "should tvReplace an int" $ tvReplace tvenv0 0 TInt == (Right $ tvExt tvenv0 0 TInt)
    it "should not tvReplace a self referential type" $ shouldBeLeft $ tvReplace tvenv0 0 (TV 0 :> TV 0)
    itShouldTypecheckTo "1" TInt
    itShouldTypecheckTo "(\\x -> 1) 2" TInt
    itShouldTypecheckTo "let x = 1 in x" TInt
    itShouldThrowTypeError "1 1"
    itShouldThrowTypeError "x"
    itShouldThrowTypeError "let x = 1 in x x"
    itShouldTypecheckTo "let id = \\x -> x in id 1" TInt
    itShouldTypecheckTo "let id = \\x -> x in id id 1" TInt
    itShouldThrowTypeError "\\x -> x x"
    itShouldTypecheckTo "1" TInt
    itShouldThrowTypeError "let y = \\x -> x x in y (\\x -> x)"
    

