{-# LANGUAGE LambdaCase #-} 

module SubsToFile where
import Control.Arrow
import Types
import General

writeTo = writeFile

subsToFile :: Subjects -> IO ()
subsToFile = 
  subsToString >>> (writeTo dataFile)

subsToString :: Subjects -> String
subsToString =
  myFoldr subToString

subToString :: Subject -> String
subToString = \(name,done,todo) -> 
  name ++ "\n" ++ myFoldr exsToString [done,todo]
  
exsToString :: Exercises -> String
exsToString = myFoldr exToString

exToString
  
