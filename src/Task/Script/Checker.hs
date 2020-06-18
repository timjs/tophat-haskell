module Task.Script.Checker where

import Task.Script.Syntax

---- Types ---------------------------------------------------------------------

type Context = HashMap Name Tipe

data Error

---- Checker -------------------------------------------------------------------
check :: Context -> Expression -> Type -> Either Error ()
check g e t = _
