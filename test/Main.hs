module Main where

import Test.QuickCheck
import LambdaParserTest
import LambdaTypes
import LambdaTypeGenerator
import System.Random

main = quickCheck prop_idemparseo
