module Main where

import LambdaTypes
import LambdaParser

main = do let x = LambdaFunction {variables = Parametros ["r"], cuerpo = LambdaVariable "q"}
          print $ show x
          print (x == read (show x))
