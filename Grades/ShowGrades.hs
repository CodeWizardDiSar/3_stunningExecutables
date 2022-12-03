module ShowGrades where

import Data.List
import Data.List.Split

element n x = x!!(n-1)
gradesCsv = "grades.csv"

kormos =
  filter (\x -> element 1 x == "k")
roes =
  filter (\x -> element 1 x == "r") 
elefthera =
  filter (\x -> element 1 x == "e") 
passed =
  filter (\x -> element 4 x /= "0")
semester n =
  filter (\x -> element 2 x == show n) 
roh r =
  filter (\x -> element 2 x == r)

nameGrade subject = concat [ "\t", name, ": ", grade, "\n" ] where
  name  = element 3 subject 
  grade = element 4 subject 

toNicer filterF = concat . map nameGrade . filterF

nicerSemester = toNicer . semester
titledSemester n =
  (concat [ "\n", show n, "ο εξάμηνο:\n\n" ] ++) . nicerSemester n 

nicerRoh = toNicer . roh
titledRoh r =
  (concat [ "\nΡοή ", r, ":\n\n" ] ++) . nicerRoh r 
titledElefthera =
  ("\nΕλεύθερα:\n\n" ++) . toNicer elefthera

subjectsOf =
  map (splitOn ",") . lines
nicerKormos csv =
  concat $ map (\n -> titledSemester n $ subjectsOf csv) [ 1..5 ]
nicerRoes csv =
  concat $ map (\s -> titledRoh s $ subjectsOf csv) [ "Υ", "Λ", "Μ", "Φ" ]
nicerElefthera csv =
  titledElefthera $ subjectsOf csv

moreInfo csv = concat
  [ "\nΓενικές Πληροφορίες:\n\n" 
  , "\tΠερασμένα Κορμού: ", show $ l_p_k
  , ", Απομένουν από Κορμό: ", show $ 35 - l_p_k
  , "\n\tΠερασμένα Ροών: ", show $ l_p_r
  , ", Απομένουν από Ροές: ", show $ 22 - l_p_r
  , "\n\tΠερασμένα Ελεύθερα: ", show $ l_p_e
  , ", Απομένουν από Ελεύθερα: ", show $ 4 - l_p_e
  , concat $ map grade [ 5, 6, 7, 8, 9, 10 ] 
  , "\n\tΜέσος Όρος Περασμένων: "
  , (show $ fromIntegral (sum grades) / fromIntegral (length grades)) 
  , "\n\n"
  ]
  where
  subjects = subjectsOf csv
  numGrades = \x -> element 4 x `elem` map show [0..10]
  grades = map (\x -> read $ element 4 x) $ filter numGrades $ passed subjects
  grade n = concat ["\n\t",show n,"άρια: ",show $ length $ filter (== n) grades]
  l_p_k = length $ passed $ kormos subjects 
  l_p_r = length $ passed $ roes subjects 
  l_p_e = length $ passed $ elefthera subjects 
