gradesCsv = "grades.csv"
 
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
