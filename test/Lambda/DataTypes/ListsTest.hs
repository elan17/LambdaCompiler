module Lambda.DataTypes.ListsTest where

import Lambda
import Lambda.DefaultBindings
import Lambda.DataTypes.Lists

import Test.Hspec
import Test.QuickCheck

import Lambda.Internals.LambdaTypeGenerators.LambdaTypeGenerator

prop_head :: [LambdaTerm] -> Bool
prop_head [] = True
prop_head l = reduceDefault (head l) == reduceDefault (headl <\ fromList l)

prop_tail :: [LambdaTerm] -> Bool
prop_tail [] = True
prop_tail [_] = True
prop_tail l = reduceDefault (fromList (tail l)) == reduceDefault (taill <\ fromList l)

listsTest =
    describe "Lambda.DataTypes.Lists" $ do
    it "Comprobando HEAD" $
        property prop_head
    it "Comprobando TAIL" $
        property prop_tail
