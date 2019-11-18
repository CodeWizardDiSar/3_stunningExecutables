module Types where
import Control.Monad.State

type MaybeExName       = Maybe String

type ToDo              = Exercises 
type Done              = Exercises

type SubName           = String
type Line              = String

type Exercise          = (MaybeExName,Int)
type Subject           = (SubName,Done,ToDo)

type Path              = String
type Lines             = [Line]
type SLName            = State Lines SubName
type SLDone            = State Lines Done
type SLToDo            = State Lines ToDo
type SLSub             = State Lines Subject
type SLSubs            = State Lines Subjects
type SLEx              = State Lines Exercise
type SLExs             = State Lines Exercises

type Exercises         = [Exercise] 
type Strings           = [String] 
type Subjects          = [Subject] 
