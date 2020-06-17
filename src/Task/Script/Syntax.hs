module Task.Script.Syntax where

---- Synonyms ------------------------------------------------------------------

type Name = Text

type Label = Text

type Message = Text

---- Expressions ---------------------------------------------------------------

data Expr

data Prim
  = B Bool
  | I Int
  | S Text

data Match
  = MIgnore
  | MVar Name
  | MRecord (HashMap Name Match)

---- Statements ----------------------------------------------------------------

data Stmt
  = SBind Match Task Stmt
  | STask

data Args
  = ARecord (HashMap Name Expr)

data Task
  = -- Editors
    Edit Message
  | Update Message Expr
  | Change Message Expr
  | View Message Expr
  | Watch Message Expr
  | -- Basics
    Done Expr
  | Pair (List Stmt)
  | Choose (List Stmt)
  | Branch (List (Expr, Stmt))
  | Select (List (Label, Expr, Stmt))
  | -- Extras
    Apply Name Args
  | Hole Args
  | -- Shares
    Share Expr
  | Assign Expr Expr
