{-# LANGUAGE LambdaCase #-} 
module Paths where
import Types
import Renaming

homeDir           ="/home/gnostis"
githubDir         = homeDir`append`"/Desktop/2Github"
exercisesDir      = githubDir`append`"/3StunningExecutables/1Exercises"
dataDir           = exercisesDir`append`"/0Data"
versionKeeper     = dataDir`append`"/ver"   
tempVersionKeeper = dataDir`append`"/verTmp"
dataKeeperPrefix  = dataDir`append`"/data"  

exercisesDir      :: FilePath
dataDir           :: FilePath
versionKeeper     :: FilePath
tempVersionKeeper :: FilePath
dataKeeperPrefix  :: FilePath
