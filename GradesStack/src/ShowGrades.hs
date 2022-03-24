module ShowGrades where

import Data.List
import Data.List.Split

from x = x 
element n from x = from x!!(n-1)
gradesCsv = "grades.csv"

kormos     = filter (\x -> element 1 from x == "k")
roes       = filter (\x -> element 1 from x == "r") 
elefthera  = filter (\x -> element 1 from x == "e") 
passed     = filter (\x -> element 4 from x /= "0")

passedKormos    = passed . kormos
passedRoes      = passed . roes
passedElefthera = passed . elefthera

semester n = filter (\x -> element 2 from x == show n) 
roh      r = filter (\x -> element 2 from x == r     )

nameGrade subject = concat ["\t",name,": ",grade,"\n"]
  where name  = element 3 from subject 
        grade = element 4 from subject 
toNicer   filterF = concat . map nameGrade . filterF

nicerSemester    = toNicer . semester
titledSemester n = (concat ["\n",show n,"ο εξάμηνο:\n\n"] ++) . nicerSemester n 

nicerRoh    = toNicer . roh
titledRoh r = (concat ["\nΡοή ",r,":\n\n"] ++) . nicerRoh r 

titledElefthera = (concat ["\nΕλεύθερα:\n\n"] ++) . toNicer elefthera

subjectsOf = map (splitOn ",") . lines
nicerKormos csv = concat $ map (\n -> titledSemester n (subjectsOf csv)) [1,2,3,4,5]
nicerRoes csv = concat $ map (\s -> titledRoh s (subjectsOf csv)) ["Υ","Λ","Μ","Φ"]
nicerElefthera csv = titledElefthera (subjectsOf csv)

moreInfo csv = concat [
    "\nΓενικές Πληροφορίες:\n\n" 
  , "\tΠερασμένα Κορμού: "  , (show $ l_p_k) 
  , ", Απομένουν από Κορμό: ", (show $ 35 - l_p_k) 
  , "\n\tΠερασμένα Ροών: " , (show $ l_p_r) 
  , ", Απομένουν από Ροές: " , (show $ 22 - l_p_r) 
  , "\n\tΠερασμένα Ελεύθερα: " , (show $ l_p_e) 
  , ", Απομένουν από Ελεύθερα: " , (show $ 4 - l_p_e) 
  , (concat (map grade [5,6,7,8,9,10])) 
  , "\n\tΜέσος Όρος Περασμένων: "
  , (show $ fromIntegral (sum grades) / fromIntegral (length grades)) 
  , "\n\n"
  ]
  where subjects = subjectsOf csv
        numGrades = \x -> element 4 from x `elem` map show [0..10]
        grades = map (\x -> read $ element 4 from x) $ filter numGrades $ passed subjects
        grade n = concat ["\n\t",show n,"άρια: ",show $ length $ filter (== n) grades]
        l_p_k = length $ passedKormos subjects 
        l_p_r = length $ passedRoes subjects 
        l_p_e = length $ passedElefthera subjects 
