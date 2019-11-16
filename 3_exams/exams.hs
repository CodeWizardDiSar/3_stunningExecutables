import Data.List
import Data.List.Split

from n = n
number n = (!!(n-1))
extractExs = map (splitOn ",") . lines

before ex1 ex2 = (m1 > m2) || (m1 == m2) && (d1 >  d2)
  where readInts = map (\x -> (read x) :: Int)
        dateFrom = readInts . splitOn "/" . number 2 . from 
        [d1,m1] = dateFrom ex1
        [d2,m2] = dateFrom ex2

sortChrono [] = []
sortChrono (ex1:exs) = (sortChrono before_) ++ [ex1] ++ (sortChrono later)
  where (before_,later) = partition (before ex1) exs

toNicerForm [subject,date,time] = concat ["\t",subject," -> ",date," --- ",time,"\n"] 

main = interact (\csv -> concat ["\nΕξεταστική:\n\n",nicerDisplayThan csv,"\n"])
    where nicerDisplayThan = concat . map toNicerForm . sortChrono . extractExs
