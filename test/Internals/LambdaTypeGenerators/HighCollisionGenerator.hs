module Internals.LambdaTypeGenerators.HighCollisionGenerator where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import CompiladorLambda

import Control.Monad
import Control.Applicative

newtype HighCollisionLambdaType = HighCollisionLambdaType LambdaTerm
    deriving (Show, Eq)

-- | Instancia que provoca muchas colisiones de nombres de variables
instance Arbitrary HighCollisionLambdaType where
    arbitrary = HighCollisionLambdaType <$> sized arbitrary'
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

              varStringGen = fmap (:[]) (oneof (map return "abcd"))

              stubGen = fmap LambdaStub (listOf1 (oneof (map return "ABCD")))
