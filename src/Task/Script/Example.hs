module Task.Script.Example where

import Task.Script.Syntax

---- Types ---------------------------------------------------------------------

t_nationality :: Ty
t_nationality =
  TVariant
    [ "Dutch" ~> TRecord [],
      "British" ~> TRecord [],
      "German" ~> TRecord []
    ]

t_passenger :: Ty
t_passenger =
  TRecord
    [ "first_name" ~> TPrimitive TString,
      "last_name" ~> TPrimitive TString,
      "nationality" ~> t_nationality,
      "age" ~> TPrimitive TInt
    ]

t_flight :: Ty
t_flight =
  TVariant
    [ "ToAmsterdam" ~> TRecord [],
      "ToLondon" ~> TRecord [],
      "ToBerlin" ~> TRecord []
    ]

t_row :: Ty
t_row = TPrimitive TInt

t_chair :: Ty
t_chair = TPrimitive TString

t_seat :: Ty
t_seat =
  TRecord
    [ "row" ~> t_row,
      "chair" ~> t_chair
    ]

t_booking :: Ty
t_booking =
  TRecord
    [ "passengers" ~> TList t_passenger,
      "flight" ~> t_flight,
      "seats" ~> TList t_seat
    ]

---- Tasks ---------------------------------------------------------------------

enter_passenger :: Statement
enter_passenger =
  Step (MRecord ["value" ~> MBind "passengers"]) (Enter (BPrimitive TString) "Passenger details")
    <| Task
    <| Select
      [ ( "Continue",
          Constant (B True),
          Task <| Done (Record ["passengers" ~> Variable "passengers"])
        )
      ]
