{-# LANGUAGE FlexibleInstances #-}
module LambdaTypeGenerator where

import Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import LambdaTypes
import           Control.Monad
import           Data.List.Utils


instance Arbitrary (LambdaTerm String) where
    arbitrary = sized arbitrary'
        where arbitrary' 0 = varGen
              arbitrary' depth = oneof [ varGen
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

              varStringGen :: Gen String
              varStringGen = listOf1 $ oneof (map return "abcdefghijklmnopqrstuvwxyz")
