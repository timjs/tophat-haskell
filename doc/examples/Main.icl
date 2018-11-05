module Main


import iTasks



// Types ///////////////////////////////////////////////////////////////////////


:: Passenger =
  { name :: String
  , age :: Int
  }


:: Seat  = Seat Row Chair
:: Row   :== Int
:: Chair :== Char


:: Booking =
  { passengers :: [Passenger]
  , seats :: [Seat]
  }



// Helpers /////////////////////////////////////////////////////////////////////


remove :: a [a] -> [a] | iTask a
remove x [y:ys]
  | x === y   = ys
  | otherwise = [y : remove x ys]
remove x []   = []


removeElems :: [a] [a] -> [a] | iTask a
removeElems []     ys = ys
removeElems [x:xs] ys = removeElems xs (remove x ys)



// Stores //////////////////////////////////////////////////////////////////////


initSeats :: [Seat]
initSeats =
  [ Seat r p \\ r <- [1..4], p <- ['A'..'C'] ]



// Checks //////////////////////////////////////////////////////////////////////


valid :: Passenger -> Bool
valid p = p.age >= 0


adult :: Passenger -> Bool
adult p = p.age >= 18



// Tasks ///////////////////////////////////////////////////////////////////////


enterPassengers :: String -> Task [Passenger]
enterPassengers username =
  enterInformation ( username, "Passenger details" ) [] >>?
    [ ( "Continue"
      , \passengers -> all valid passengers && any adult passengers && not (isEmpty passengers)
      , return
      )
    ]


chooseSeats :: String (Shared [Seat]) Int -> Task [Seat]
chooseSeats username freeSeats n =
  enterMultipleChoiceWithShared ( username, "Pick a seat" ) [] freeSeats >>?
    [ ( "Pick"
      , \seats -> length seats == n
      , \seats ->
          freeSeats $= removeElems seats >>- \_ ->
          return seats
      )
    ]


makeBooking :: String (Shared [Seat]) -> Task Booking
makeBooking username freeSeats =
  enterPassengers username >>- \passengers ->
  chooseSeats username freeSeats (length passengers) >>- \seats ->
  viewInformation ( the username, "Booking" ) [] { passengers = passengers, seats = seats }
  where
    // Make sure the argument is a String
    the :: String -> String
    the x = x


main :: Task ( Booking, ( Booking, Booking ) )
main =
  withShared initSeats (\freeSeats ->
    (makeBooking "User 1" freeSeats -&&- (makeBooking "User 2" freeSeats -&&- makeBooking "User 3" freeSeats
      <<@ ArrangeHorizontal) <<@ ArrangeHorizontal)
  )



// Boilerplate /////////////////////////////////////////////////////////////////


derive class iTask Seat, Booking, Passenger


Start :: *World -> *World
Start world = startEngine main world


(>>?) infixl 1 :: (Task a) [( String, a -> Bool, a -> Task b )] -> Task b | iTask a & iTask b
(>>?) task options = task >>* map trans options
where
  trans ( a, p, t ) = OnAction (Action a) (ifValue p t)


($=) infixr 2 :: (ReadWriteShared r w) (r -> w) -> Task w | iTask r & iTask w
($=) share fun = upd fun share
