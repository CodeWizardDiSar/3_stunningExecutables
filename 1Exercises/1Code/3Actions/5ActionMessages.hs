{-# LANGUAGE LambdaCase #-} 
module ActionMessages where
import Useful
import Messages

shotm  = "Show"      -- SHOw      Title Message
addtm  = "Add to"    -- ADD       Title Message
chatm  = "Change"    -- CHAnge    Title Message
movftm = "Move from" -- MOVe From Title Message
movttm = "Move from" -- MOVe To   Title Message
tdom   = "a: To Do"  -- To DO  Message
donm   = "s: Done"   -- DONe   Message
msdm   = "d: Missed" -- MiSseD Message
allm   = "f: All"    -- ALL    Message

optms  = [tdom,donm,msdm,exim]       -- OPTions MessageS
sopms  = take 3 optms ++ [allm,exim] -- Show OPtions MessageS
shoms  = msf shotm  sopms            -- SHOw MessageS
addms  = msf addtm  optms            -- ADD MessageS
movfms = msf movftm optms            -- MOVe MessageS
movtms = msf movftm optms            -- MOVe MessageS
