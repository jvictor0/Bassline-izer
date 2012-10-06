module Note where

import Utils
import Data.List

data NoteValue = C | Db | D | Eb | E | F | Gb | G | Ab | A | Bb | B 
          deriving (Ord, Enum, Eq, Show, Read)
data Length = Whole | Half | Quarter | Eighth | Triplet | Sixteenth  
            deriving (Eq,Show,Ord,Read)
data Note = Note NoteValue Int deriving (Eq)

numNotes = 12

noteValue :: Note -> NoteValue
noteValue (Note n _) = n
noteRange :: Note -> Int
noteRange (Note _ i) = i

distance :: Note -> Note -> Int
distance a b = abs $ (fromEnum b) - (fromEnum a)

nearestNote :: Note -> NoteValue -> Note
nearestNote c n = minimumBy (compare `on` (distance c)) [Note n (noteRange c), Note n $ (noteRange c)-1, Note n $ (noteRange c) +1]

instance Ord Note where
  compare (Note n i) (Note m j) = case compare i j of
    EQ -> compare n m
    result -> result
              
instance Show Note where
  show nt = show $ noteValue nt
  
instance Enum Note where
  fromEnum (Note n i) = numNotes*i+(fromEnum n)
  toEnum i = Note (toEnum $ i`mod`numNotes) (i`div`numNotes)
  


