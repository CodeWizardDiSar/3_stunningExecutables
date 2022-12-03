{-# language LambdaCase #-}

module VeryUseful.UsefulFunctions where

import Prelude 
  ( IO, (+), (-), sequence_, (++), (>>), String, Int )
import Data.Function
  ( (&) )
import Types
  ( Strings )
import VeryUseful.Renaming
  ( (.>), wrap, forEachDo, repeatNTimes, printErrorMessage , glue )
import TypeClasses.FromString
  ( fromString )
import TypeClasses.ToString
  ( toString, print )

doSequentially :: [ IO () ] -> IO ()
doSequentially = sequence_

tabBefore :: String -> String
tabBefore = 1 & tabsBefore

tabsBefore :: Int -> String -> String
tabsBefore = \i s-> ( repeatNTimes i '\t' ) ++ s

printStrings :: Strings -> IO ()
printStrings = forEachDo print

printEmptyLine :: IO ()
printEmptyLine = print ""

showConfusion :: IO ()
showConfusion = print "I'm not sure I understand :/\n"

printEmptyLines :: Int -> IO ()
printEmptyLines = \case
  0 -> wrap ()
  i -> printEmptyLine >> ( i - 1 & printEmptyLines )

addNToString :: Int -> String -> String
addNToString = \n -> fromString .> ( + n ) .> toString

addOneToString :: String -> String
addOneToString = addNToString 1

subOneFromString :: String -> String
subOneFromString = addNToString (-1)
