module UsefulForActions where
import Types 
import Renaming            (append,wrap,andThen,and,unwrapAnd,
                            glue,forEach,printString,
                            convertIntToString,
                            convertIntFromString)
import Prelude             (sequence,not,(<),(.),filter,(&&),
                            (>),(||),(==),repeat,take,length,
                            Bool(..),getLine,Int,IO,(+),($),
                            elem,(-),(!!))
import StringFromExercises (exercisesToString)
import FileManagement      (writeToNextDataKeeper)
import Data.Function       ((&))
import Control.Monad       ((>=>))

beautify        = ("\t"`append`)`and`(`append`"\n")
putTogether     = forEach ((`append`repeat ' ')`and`take 25)
                  `and`glue
printBeutified  = beautify`and`printString
exercisesToFile = exercisesToString`and`writeToNextDataKeeper
getChoice       = getLine`unwrapAnd`(convertIntFromString`and`
                                     wrap)
combine         = sequence>=>(glue`and`wrap) ::
                  [IO Exercises]->IO Exercises

-- Show Subjects
showSubjects = getSubjects`and`printSubjects 1
getSubjects  = \case []     -> []
                     ex:exs -> let sub  = ex&getSub
                                   subs = exs&getSubjects
                               in elem sub subs& \case
                                  True -> subs
                                  _    -> sub:subs
printSubjects = \i-> \case []      ->wrap ()
                           sub:subs->([i&convertIntToString,
                                       ": ",sub]&glue&
                                       printString)`andThen`
                                       printSubjects (i+1) subs
getSub = getData`and` \(s,_,_)->s

-- Sort Chronologically
sortChrono = \case []    -> [] 
                   ex:exs-> sortChrono (filter (before ex) exs)
                            `append`[ex]`append`
                            sortChrono (filter (not.before ex)
                                        exs)
before   = \e1 e2->getTDate e2`isBefore`getTDate e1
isBefore = \[d,m,y] [d',m',y']->y<y'||(y==y'&&m<m')||
                                (y==y'&&m==m'&&d<d')

askFor = \s->printString s`andThen`getLine`unwrapAnd`\a->
 case length a>20 of True->printString annoyingMessage`andThen`
                           askFor s
                     _   ->wrap a

annoyingMessage = "More than 20 chars is not pretty"
