module ShowToDo where
import Control.Arrow
import FileToSubs
import Types
import General

formatMsg="\tSubject | Exercise Number | Exercise Name | Date"

std=nli>>pst formatMsg>>nli>>toDo
toDo=(fts>>=(map tts>>>cnc>>>pst))::IOU

instance TTS SUB where
  tts (Sub n _ t)=(map ((cnc ["\t\t",n,", "]++).toDoStr)>>>cnc) t

toDoStr (Tex na n (d,m)) =
  cnc [cts n
      ,", "
      ,case na of Nng->"Not Important";Idd n->n
      ,", "
      ,cts d
      ,"/"
      ,cts m
      ,"\n"]
