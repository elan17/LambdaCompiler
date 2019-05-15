{-|
Module      : Lambda.Operations.Logic
Description : Módulo para operaciones booleanas
Copyright   : (c) Juan Toca, 2019
License     : GPL-3
Maintainer  : elan17.programacion@gmail.com
Stability   : experimental
Portability : POSIX

Módulo definiendo las expresiones básicas de las operaciones booleanas con
una codificación Church
-}
module Lambda.Operations.Logic
    ( verdadero
    , falso
    , puertaAND
    , puertaOR
    , puertaNOT
    , bindings
    , (D.&&)
    , (D.||)
    , (!)
    , toBoolean
    , fromBoolean
    )
where

import           Lambda.Operations.Logic.Internals.Definition as D
