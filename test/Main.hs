module Main where

import Test.QuickCheck
import Test.Hspec
import Internals.LambdaParserTest
import Internals.Trasformations.AlphaTransformationTest
import Internals.LambdaTypeGenerator

main = hspec $ do
        describe "Internals.LambdaParser" $ do
            it "Parsea correctamente expresiones lambda arbitrarias" $ do
                property $ prop_idemparseo
        describe "Internals.AlphaTransformation.alphaTransform" $ do
            it "Mantiene la estructura de la expresion lambda" $ do
                property $ prop_mismaEstructura
            it "No se repiten nombres de variables en los parámetros" $ do
                property $ prop_sinParametrosRepetidos
            it "Las variables unbound se mantienen con los mismos nombres" $ do
                property $ prop_unboundIguales
