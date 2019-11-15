{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module ForEverybody where

type Subject = String
data Exercise =
  InfoToExercise { number  :: Int,
                   subject :: Subject }
type Exercises = [Exercise] 
type Done = Exercises
type Todo = Exercises 
type Strings = [String] 

getNumber :: IO Int
getNumber = do
  snum <- getLine
  return $ read snum

printString = putStrLn
printStrings :: Strings -> IO ()
printStrings = mapM_ putStrLn
newline = printString ""
