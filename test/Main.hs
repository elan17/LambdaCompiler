module Main where

import Test.QuickCheck
import Test.Hspec
import Lambda.Internals.LambdaParserTest
import Lambda.Internals.Transformations.AlphaTransformationTest
import Lambda.Internals.Transformations.BetaTransformationTest
import Lambda.Internals.LambdaTypeGenerators.LambdaTypeGenerator
import Lambda.Operations.Arithmetics.IntegersTest
import Lambda.Operations.Logic.LogicTest
import Lambda.DataTypes.ListsTest

main = hspec $ do
        parserTest
        alphaTest
        betaTest
        integersTest
        logicTest
        listsTest
