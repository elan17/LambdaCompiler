{-|
Module      : Lambda.Operations.Integers
Description : Módulo para calculo de enteros
Copyright   : (c) Juan Toca, 2019
License     : GPL-3
Maintainer  : elan17.programacion@gmail.com
Stability   : experimental
Portability : POSIX

Módulo que permite expresar y realizar cálculos con números utilizando la
codificación Church
-}
module Lambda.Operations.Arithmetics.Integers
    ( zero
    , succesor
    , plus
    , mul
    , bindings
    , D.toInteger
    )
where

import           Lambda.Operations.Arithmetics.Integers.Internals.Definition as D
