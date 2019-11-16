import Data.List
import Data.List.Split

from x = x 
element n from x = from x!!(n-1)
gradesCsv = "/home/gnostis/Dropbox/2_Hobbies/1_Prog/1_Projects/1_UniRelated/2_Grades/grades.csv"

kormos = filter (\x -> element 1 from x == "k")
roes   = filter (\x -> element 1 from x == "r") 
passed = filter (\x -> element 4 from x /= "0")

passedKormos = passed . kormos
passedRoes   = passed . roes

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

subjectsOf = map (splitOn ",") . lines
nicerKormos csv = concat $ map (\n -> titledSemester n (subjectsOf csv)) [1,2,3,4,5]
nicerRoes csv = concat $ map (\s -> titledRoh s (subjectsOf csv)) ["Υ" ,"Λ","Μ"]

moreInfo csv = 
  "\nΓενικές Πληροφορίες:\n\n" ++
  "\tΠερασμένα Κορμού: "  ++ (show $ l_p_k) ++
  "\n\tΑπομένουν από Κορμό: "++ (show $ 35 - l_p_k) ++
  "\n\tΠερασμένα Ροών: " ++ (show $ l_p_r) ++
  "\n\tΑπομένουν από Ροές: " ++ (show $ 25 - l_p_r) ++
  (concat (map grade [5,6,7,8,9,10])) ++
  "\n\tΜέσος Όρος Περασμένων: " ++ (show $ fromIntegral (sum grades) / fromIntegral (length grades)) ++
  "\n\n"
  where subjects = subjectsOf csv
        grades = map (\x -> read $ element 4 from x) $ passed subjects
        grade n = concat ["\n\t",show n,"άρια: ",show $ length $ filter (== n) grades]
        l_p_k = length $ passedKormos subjects 
        l_p_r = length $ passedRoes subjects 

main = do
  csv <- readFile gradesCsv
  putStr $ concat [nicerKormos csv, nicerRoes csv, moreInfo csv]
