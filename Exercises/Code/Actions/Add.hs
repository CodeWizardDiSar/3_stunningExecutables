module Add where
import Prelude
  ( (>>=), (>>), IO )
import Types
  ( Exercise( ToDo, Done, Missed ) )
import Renaming
  ( (.>) )
import ExercisesFromFile
  ( getExercisesFromFile )
import FileManagement
  ( updateVersion )
import UsefulForActions
  ( writeExercisesToFile )
import GetFromUser
  ( getFromUser )

addActions :: [ IO () ]
addActions =
  [ getFromUser >>= ToDo .> add, getFromUser >>= Done .> add, getFromUser >>= Missed .> add ]

add :: Exercise -> IO ()
add exerciseFromUser =
  getExercisesFromFile >>= ( exerciseFromUser : ) .> writeExercisesToFile >> updateVersion
