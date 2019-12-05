module ShowToDo where
import Control.Arrow
import FileToSubs
import Types
import General

formatMsg = "\tSubject | Exercise Number | Exercise Name | Date"

showToDo = do
  nli
  pst formatMsg
  nli
  toDo

toDo = fileToSubs >>= (map toDoFromSub >>> concat >>> pst)

toDoFromSub (SU n _ t) =
  (map (toDoStr >>> (("\t\t" ++ n ++ ", ") ++)) >>> concat) t

toDoStr (TE na n (d,m)) =
  show n ++ ", " ++
  (case na of Nothing -> "Not Important"; Just n -> n) ++ ", " ++
  show d ++ "/" ++ show m ++ "\n"
