module IsEarlierThan where
import Prelude
  ( Bool, Int, (<), (||), (==), (&&) )
import Types
  ( Exercise( ToDo ), ToDoExercise( date ), Date( Date ), Day( Day ), Month( Month )
  , Year( Year ) )
import Renaming
  ( printErrorMessage )

class IsEarlierThan a where isEarlierThan :: a -> a -> Bool

instance IsEarlierThan Exercise where 
  e1 `isEarlierThan` e2 = case ( e1, e2 ) of
    ( ToDo toDo1, ToDo toDo2 ) -> date toDo1 `isEarlierThan` date toDo2
    _ -> printErrorMessage
          "Programmer messed up: trying to sort chronologically non-ToDo exercise"

instance IsEarlierThan Date where 
  date1 `isEarlierThan` date2 = toInts date1 `isEarlierThan` toInts date2

toInts :: Date -> Ints
toInts ( Date ( Day day ) ( Month month ) ( Year year ) ) = [ day, month, year ]

type Ints = [ Int ]
instance IsEarlierThan Ints where 
  ints1 `isEarlierThan` ints2 = case ( ints1, ints2 ) of
    ( [ day1, month1, year1 ], [ day2, month2, year2 ] ) ->
      year1 < year2 || ( year1 == year2 && month1 < month2 )
                    || ( year1 == year2 && month1 == month2 && day1 < day2 )
    _ -> printErrorMessage
          "Programmer messed up: trying to sort chronologically length != 3 ints"

