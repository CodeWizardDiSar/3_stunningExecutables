gradesCsv = "/home/gnostis/Dropbox/2_Hobbies/1_Prog/1_Projects/1_UniRelated/2_Grades/grades.csv"
 
main = do
  putStrLn "Εισάγετε 'k' για κορμό ή 'r' για ροή" 
  subType <- getLine
  putStrLn "Εισάγετε εξάμηνο ή ροή" 
  semOrRoh <- getLine
  putStrLn "Εισάγετε τίτλο μαθήματος" 
  title <- getLine
  putStrLn "Εισάγετε βαθμό" 
  grade <- getLine
  let csvFormat = concat [subType,",",semOrRoh,",",title,",",grade,"\n"]
  appendFile gradesCsv csvFormat
