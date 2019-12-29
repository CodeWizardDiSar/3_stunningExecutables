{-# LANGUAGE LambdaCase #-} 
module Paths where
import Renaming (append)

homeDir           = "/home/gnostis"
githubDir         = homeDir     `append`"/Desktop/2Github"
exercisesDir      = githubDir   `append`"/3StunningExecutables/1Exercises"
dataDir           = exercisesDir`append`"/3Data"
versionKeeper     = dataDir     `append`"/ver"   
tempVersionKeeper = dataDir     `append`"/verTmp"
dataKeeperPrefix  = dataDir     `append`"/data"  
