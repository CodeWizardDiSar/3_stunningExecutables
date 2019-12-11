module ShowToDo where
import Control.Arrow
import Data.Time.Calendar
import Data.Time.Clock
import FileToSubs
import Types
import General

date=getCurrentTime >>= return . toGregorian . utctDay >>= print
fom="\tSubject | Exercise Number | Exercise Name | Remaining Days"
nim="Not Important"

std=nli>>pst fom>>nli>>ptd
ptd=(fts>>=(map tts>>>cnc>>>pst))::IOU
tdd=[", ",", ","/","\n"]

instance TTS SUB where tts (Sub n _ t)=(map (tte n)>>>cnc) t
tte n=tes>>>dew tdd>>>pnf n--To To do Exercise
pnf n=(cnc ["\t\t",n,", "]++)--Put Name in Front
tes (Tex na n (d,m))--To do Exercise Strings
  =[cts n,case na of Nng->nim;Idd n->n,cts d,cts m]
