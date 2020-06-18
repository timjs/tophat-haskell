module Task.Script.Syntax
  ( -- * Synonyms
    Row,
    Name,
    Label,
    Message,

    -- * Types
    Tipe (..),
    PrimTipe (..),
    isBasic,

    -- * Expressions
    Expression (..),
    Constant (..),

    -- * Matches
    Match (..),

    -- * Statements
    Statement (..),
    Task (..),
  )
where

---- Synonyms ------------------------------------------------------------------

type Row a = HashMap Label a

type Name = Text

type Label = Text

type Message = Text

---- Types ---------------------------------------------------------------------

data Tipe
  = TFunction Tipe Tipe
  | TRecord (Row Tipe)
  | TReference Tipe
  | TTask Tipe
  | TPrimitive PrimTipe

data PrimTipe
  = TBool
  | TInt
  | TString

isBasic :: Tipe -> Bool
isBasic = \case
  TPrimitive _ -> True
  TRecord r -> all isBasic r
  TFunction _ _ -> False
  TReference _ -> False
  TTask _ -> False

---- Expressions ---------------------------------------------------------------

data Expression
  = Lambda Match Tipe Expression
  | Apply Expression Expression
  | Variable Name
  | Location Nat
  | IfThenElse Expression Expression Expression
  | Case Expression (List (Label, Match, Tipe, Expression))
  | Record (Row Expression)
  | Constant Constant

data Constant
  = B Bool
  | I Int
  | S Text

---- Matches -------------------------------------------------------------------

data Match
  = MIgnore
  | MBind Name
  | MRecord (Row Match)
  | MUnpack

---- Statements ----------------------------------------------------------------

data Statement
  = SBind Match Task Statement
  | STask

data Task
  = -- Editors
    Edit Message
  | Update Message Expression
  | Change Message Expression
  | View Message Expression
  | Watch Message Expression
  | -- Basics
    Done Expression
  | Pair (List Statement)
  | Choose (List Statement)
  | Branch (List (Expression, Statement))
  | Select (List (Label, Expression, Statement))
  | -- Extras
    Execute Name (Row Expression)
  | Hole (Row Expression)
  | -- Shares
    Share Expression
  | Assign Expression Expression
