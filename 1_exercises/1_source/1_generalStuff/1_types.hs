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
type SL a              = State Lines a

type Exercises         = [Exercise] 
type Strings           = [String] 
type Subjects          = [Subject] 
