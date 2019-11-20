{-# LANGUAGE LambdaCase #-} 

module SubsToFile where
import Control.Arrow
import Types
import General

subsToFile :: Subjects -> IO ()
subsToFile = 
  subsToString >>> \s ->
  nextVersFile >>= \f ->
  writeFile f s

subsToString :: Subjects -> String
subsToString =
  myFoldr subToString

subToString :: Subject -> String
subToString = \(name,done,todo) -> 
  concat [name,"\n"
         ,exsToString done
         ,"ToDo","\n"
         ,exsToString todo
         ,"SubEnd","\n"
         ]

exsToString :: Exercises -> String
exsToString =
  myFoldr exToString

exToString :: Exercise -> String
exToString = \(maybeExName,num) ->
  concat [maybeExNameToString maybeExName," ",show num,"\n"]

maybeExNameToString :: MaybeExName -> String
maybeExNameToString =
  \case Nothing     -> "NoName"
        Just exName ->  exName 
