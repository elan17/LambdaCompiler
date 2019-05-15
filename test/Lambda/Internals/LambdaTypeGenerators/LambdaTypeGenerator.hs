{-# LANGUAGE FlexibleInstances #-}
module Lambda.Internals.LambdaTypeGenerators.LambdaTypeGenerator where

import Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import  Lambda
import           Control.Monad

-- | La instancia @Arbitrary@ para el dato @LambdaTerm String@ permite generar
-- casos aleatorios para comprobar las propiedades de los tests
instance Arbitrary LambdaTerm where
    arbitrary = sized arbitrary'
        where arbitrary' 0 = varGen
              arbitrary' depth = oneof [ varGen
                                       , stubGen
                                       , functionGen depth
                                       , applicationGen depth]
              applicationGen depth=
                  do x <- fmap LambdaApplication (arbitrary' (depth-1))
                     fmap x (arbitrary' (depth-1))

              functionGen depth=
                  do x <- fmap LambdaFunction paramsGen
                     fmap x (arbitrary' (depth-1))

              paramsGen = fmap Parametros (listOf1 varStringGen)

              varGen = fmap LambdaVariable varStringGen

              varStringGen = listOf1 (oneof (map return "abcdefghijklmnñopqrstuvwxyz"))

              stubGen = fmap LambdaStub (listOf1 (oneof (map return "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ+-*/")))
