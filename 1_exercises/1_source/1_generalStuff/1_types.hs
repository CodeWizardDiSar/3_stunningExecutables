module Types where
import Control.Monad.State

type Day               = Int
type Month             = Int
type Size              = Int

type SubName           = String
type ExName            = String
type Line              = String
type Decor             = Strings

data MExName           = MEN {getMEN :: Maybe ExName}

type Date              = (Day,Month)
data DoneEx            = DE MExName Int
data ToDoEx            = TE MExName Int Date
data Subject           = SU SubName DoneExs ToDoExs

type DoneExs           = [DoneEx]
type ToDoExs           = [ToDoEx]
type Strings           = [String] 
type Subjects          = [Subject] 
type Lines             = [Line]

type SL a              = State Lines a
