module Main where

import Test.QuickCheck
import Internals.LambdaParserTest
import Internals.LambdaTypeGenerator
import System.Random

main = quickCheck prop_idemparseo
