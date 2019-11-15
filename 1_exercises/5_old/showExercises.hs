module ShowExercises where

import Data.List
import Data.List.Split

from n = n
element n = (!!(n-1))
extractExs = map (splitOn ",") . lines
exercisesCsv = "/home/gnostis/Dropbox/2_Hobbies/1_Prog/1_Projects/1_UniRelated/1_Exs/exercises.csv"

before ex1 ex2 = (y1 > y2) || (y1 == y2) && (m1 > m2) || (y1 == y2) && (m1 == m2) && (d1 >  d2)
  where readInts = map (\x -> (read x) :: Int)
        dateFrom = readInts . splitOn "/" . element 3 . from 
        [d1,m1,y1] = dateFrom ex1
        [d2,m2,y2] = dateFrom ex2

sortChrono l = case l of
  [] -> []
  (ex1:exs) -> (sortChrono before_) ++ [ex1] ++ (sortChrono later)
    where (before_,later) = partition (before ex1) exs

toNicerForm [subject, goal, date] = concat ["\t", subject, ": ", goal, " -> ", date, "\n"] 

showExercises = do
  csv <- readFile exercisesCsv
  putStr $ concat ["\nΕργασίες:\n\n", nicerDisplayThan csv, "\n"] 
    where nicerDisplayThan = concat . map toNicerForm . sortChrono . extractExs
