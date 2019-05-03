module Main where

import Test.QuickCheck
import Test.Hspec
import Internals.LambdaParserTest
import Internals.Transformations.AlphaTransformationTest
import Internals.Transformations.BetaTransformationTest
import Internals.LambdaTypeGenerators.LambdaTypeGenerator

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
            it "La transformación beta debe ser un monoide" $ do
                property $ prop_Monoid
