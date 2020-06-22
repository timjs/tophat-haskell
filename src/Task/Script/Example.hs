module Task.Script.Example where

import Task.Script.Syntax

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
