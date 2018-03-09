normalise : Task a -> State -> ( Task a, State )
-- Combinators
normalise (Seq (Pure v) cont) state =
    normalise (cont v) state
normalise (Seq left cont) state =
    let
    ( newLeft, newState ) = normalise left state
    in
    ( Seq newLeft cont, newState )
normalise (Par (Pure v1) (Pure v2)) state =
    ( Pure ( v1, v2 ), state )
normalise (Par left right) state =
    let
    ( newLeft, newState )    = normalise left state
    ( newRight, newerState ) = normalise right newState
    in
    normalise (Par newLeft newRight) newerState
