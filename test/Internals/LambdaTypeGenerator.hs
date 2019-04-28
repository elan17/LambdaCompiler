{-# LANGUAGE FlexibleInstances #-}
module Internals.LambdaTypeGenerator where

import Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import CompiladorLambda
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

              paramsGen = fmap Parametros (listOf varStringGen)

              varGen = fmap LambdaVariable varStringGen

              varStringGen = fmap (:[]) (oneof (map return "abcdefghijklmnñopqrstuvwxyz")) -- Variables de un carácter para forzar colisiones

              stubGen = fmap LambdaStub (listOf1 (oneof (map return "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ+-*/")))
