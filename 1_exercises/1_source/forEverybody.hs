module ForEverybody where

type Exercises = [Exercise] 
type Strings = [String] 
type Subjects = [Subject] 

type Done = Exercises
type Todo = Exercises 

type Name = String
type MaybeName = Maybe Name

data Subject =
  InfoToSubject { name :: Name
                , todo :: Todo
                , done :: Done
                }

data Exercise =
  InfoToExercise { name :: MaybeName
                 , number :: Int
                 }

getNumber :: IO Int
getNumber = do
  snum <- getLine
  return $ read snum

askAndGet s = do
  newline
  printString s
  getLine

printString = putStrLn
printStrings :: Strings -> IO ()
printStrings = mapM_ printString
newline = printString ""

dataFile = "../0_data/data"

isThereAFile :: IO Bool
isThereAFile = doesFileExist dataFile

subjects :: IO Subjects
subjects = do
  answer <- isThereAFile
  case answer of
    True  -> subjectsFromFile
    False -> return []

subjectsFromFile :: IO Subjects
subjectsFromFile = do
  fileContents <- readFile dataFile
  subjectsFrom fileContents

subjectsFrom :: String -> IO Subjects
subjectsFrom c = 
  let lines c =
  subject <- parseSubjects
  parseDone
  parseExercises
  parseToDo
  parseExercises
