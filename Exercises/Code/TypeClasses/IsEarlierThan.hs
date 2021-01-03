module IsEarlierThan where

import Prelude
  ( Bool, Int, (<), (||), (==), (&&) )
import Types
  ( Exercise( ToDo ), ToDoExercise( date ), Date( Date ), Day, Month, Year )
import Renaming
  ( printErrorMessage )

class IsEarlierThan a where isEarlierThan :: a -> a -> Bool

instance IsEarlierThan Exercise where 
  e1 `isEarlierThan` e2 = case ( e1, e2 ) of
    ( ToDo toDo1, ToDo toDo2 ) -> date toDo1 `isEarlierThan` date toDo2
    _ -> printErrorMessage
          "Programmer messed up: trying to sort chronologically non-ToDo exercise"

instance IsEarlierThan Date where 
  ( Date day1 month1 year1 ) `isEarlierThan` ( Date day2 month2 year2 ) =
      year1 < year2 || ( year1 == year2 && month1 < month2 )
                    || ( year1 == year2 && month1 == month2 && day1 < day2 )
