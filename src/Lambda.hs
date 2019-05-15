{-|
Module      : Lambda
Description : Módulo para cálculo lambda
Copyright   : (c) Juan Toca, 2019
License     : GPL-3
Maintainer  : elan17.programacion@gmail.com
Stability   : experimental
Portability : POSIX

Módulo principal con las funcionalidades básicas para definir y ejecutar
expresiones lambda
-}
module Lambda
    ( LambdaTerm(..)
    , Parametros(..)
    , alphaTransform
    , betaTransform
    , reduce
    , Bindings(..)
    , (<\)
    , (\>)
    , applyFunction
    , toString
    , parseTerm
    )
where

import           Lambda.Internals.Transformations.AlphaTransformation
import           Lambda.Internals.Transformations.BetaTransformation
import           Lambda.Internals.Transformations.SubstituteStubs
import           Lambda.Internals.Transformations.Reduce
import           Lambda.Internals.Instances.Equality
import           Lambda.Internals.LambdaParser
import           Lambda.Internals.LambdaTypes


import           Lambda.Operations.Arithmetics.Integers
import           Lambda.Operations.Logic
