module Task.Script.Syntax
  ( -- * Synonyms
    Row,
    Name,
    Label,
    Message,

    -- * Types
    Ty (..),
    PrimTy (..),
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

data Ty
  = TFunction Ty Ty
  | TRecord (Row Ty)
  | TReference Ty
  | TTask Ty
  | TPrimitive PrimTy
  deriving (Eq, Ord, Debug)

instance Display Ty where
  display = \case
    TFunction t1 t2 -> unwords [display t1, "->", display t2] |> between '(' ')'
    TRecord r -> display r
    TReference t -> unwords ["Ref", display t]
    TTask t -> unwords ["Task", display t]
    TPrimitive p -> display p

data PrimTy
  = TBool
  | TInt
  | TString
  deriving (Eq, Ord, Debug)

instance Display PrimTy where
  display = \case
    TBool -> "Bool"
    TInt -> "Int"
    TString -> "String"

isBasic :: Ty -> Bool
isBasic = \case
  TPrimitive _ -> True
  TRecord r -> all isBasic r
  TFunction _ _ -> False
  TReference _ -> False
  TTask _ -> False

---- Expressions ---------------------------------------------------------------

data Expression
  = Lambda Match Ty Expression
  | Apply Expression Expression
  | Variable Name
  | IfThenElse Expression Expression Expression
  | Record (Row Expression)
  | Constant Constant
  deriving (Eq, Ord, Debug)

data Constant
  = B Bool
  | I Int
  | S Text
  deriving (Eq, Ord, Debug)

instance Display Constant where
  display = \case
    B b -> display b
    I i -> display i
    S s -> display s

---- Matches -------------------------------------------------------------------

data Match
  = MIgnore
  | MBind Name
  | MRecord (Row Match)
  | MUnpack
  deriving (Eq, Ord, Debug)

instance Display Match where
  display = \case
    MIgnore -> "_"
    MBind x -> x
    MRecord ms -> display ms
    MUnpack -> "{..}"

---- Statements ----------------------------------------------------------------

data Statement
  = SBind Match Task Statement
  | STask
  deriving (Eq, Ord, Debug)

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
  deriving (Eq, Ord, Debug)
