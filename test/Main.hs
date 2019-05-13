module Main where

import Test.QuickCheck
import Test.Hspec
import Internals.LambdaParserTest
import Internals.Transformations.AlphaTransformationTest
import Internals.Transformations.BetaTransformationTest
import Internals.LambdaTypeGenerators.LambdaTypeGenerator
import Operations.Arithmetics.IntegersTest
import Operations.Logic.LogicTest

main = hspec $ do
        describe "Internals.LambdaParser" $ do
            it "Parsea correctamente expresiones lambda arbitrarias" $ do
                property $ prop_idemparseo
        describe "Internals.Transformations.AlphaTransformation.alphaTransform" $ do
            it "Mantiene la estructura de la expresion lambda" $ do
                property $ prop_mismaEstructura
            it "No se repiten nombres de variables en los parámetros" $ do
                property $ prop_sinParametrosRepetidos
            it "Las variables unbound se mantienen con los mismos nombres" $ do
                property $ prop_unboundIguales
        describe "Internals.Trasformations.BetaTransformation.betaTransform" $ do
            it "Dado f(x) = betaTransform x, se cumple que f((t1 t2)) == f((f(t1) t2))" $ do
                property $ prop_Composition
        describe "Operations.Arithmetics.Integers" $ do
            it "Comprobando suma" $
                property $ prop_Suma
            it "Comprobando multiplicación" $ do
                property $ prop_Mul
        describe "Operations.Arithmetics.Logic" $ do
            it "Comprobando operador and" $ do
                property $ prop_and
            it "Comprobando operador or" $ do
                property $ prop_or
            it "Comprobando operador not" $ do
                property $ prop_not
