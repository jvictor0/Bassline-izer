module Chord where

import qualified Data.Map as Map
import Note
import Utils
import Data.Maybe

data Interval = Root | FlatSecond | Second | MinorThird | MajorThird |
                Fourth | Tritone | Fifth | MinorSixth | MajorSixth |
                DominantSeventh | MajorSeventh deriving (Eq,Ord,Enum,Show)
                                                        
data IntervalType = Chord | Scale | Passing | ATonal deriving (Show,Eq)

type RelativeScale = [(IntervalType,Interval)]

data ModalityType = Major | Dorian | Dominant | Harmonic | Altered | Locrian deriving (Eq,Ord,Enum)

data Modality = Modality NoteValue RelativeScale ModalityType deriving (Eq)

instance Show ModalityType where
  show Major = "M7"
  show Dorian = "-7"
  show Dominant = "7"
  show Altered = "+"
  show Harmonic = "-/+7"
  show Locrian = "-7/b5"

modalityMap = Map.fromList [("M7",major),("-7",dorian),("7",dominant),("+",altered),("-/+7",harmonic),("-7/b5",locrian)]

instance UnShow Modality where
  unShow (c:'b':rst) = case Map.lookup rst modalityMap of 
    (Just fun) -> fun (read [c,'b']) 
    Nothing -> error $ "Unshow of Modality, cannot unshow " ++ [c,'b']++ rst
  unShow (c:rst) = case Map.lookup rst modalityMap of 
    (Just fun) -> fun (read [c]) 
    Nothing -> error $ "Unshow of Modality, cannot unshow " ++ [c]++ rst

instance Show Modality where
  show (Modality n _ s) = (show n)  ++ (show s)
instance Num Interval where
  a + b = toEnum $ ((fromEnum a) + (fromEnum b))`mod`numNotes
  negate b = toEnum $ (negate $ fromEnum b)`mod`numNotes
  a * b = toEnum $ ((fromEnum a) * (fromEnum b))`mod`numNotes
  fromInteger = toEnum . (`mod`numNotes) . fromInteger 
  abs = id
  signum = id

intervalBetween :: NoteValue -> NoteValue -> Interval
intervalBetween note1 note2 = toEnum $ ((fromEnum note2) - (fromEnum note1))`mod`numNotes

above :: Interval -> NoteValue -> NoteValue
i `above` c = toEnum $ ((fromEnum i) + (fromEnum c))`mod`numNotes

below :: Interval -> NoteValue -> NoteValue
i `below` c = (-i) `above` c

isAscending :: NoteValue -> NoteValue -> Bool
isAscending n m = intervalBetween n m <= Tritone

succNote = (FlatSecond `above`)
predNote = (FlatSecond `below`)

modeRoot (Modality c _ _) = c
modeType (Modality _ _ t) = t

absoluteScale :: Modality -> [(IntervalType,NoteValue)]
absoluteScale (Modality c rs _) = map (\(inttype,intval) -> (inttype,intval `above` c)) rs

absoluteArpeggio :: Modality -> [NoteValue]
absoluteArpeggio mode = map snd $ filter ((==Chord).fst) $ absoluteScale mode

major c = Modality c majorScale Major
dorian c = Modality c dorianScale Dorian
dominant c = Modality c dominantScale Dominant
altered c = Modality c alteredScale Altered
harmonic c = Modality c harmonicScale Harmonic
locrian c = Modality c locrianScale Locrian

majorScale = [(Chord,0),(Scale,2),(Chord,4),(Scale,5),
              (Chord,7),(Passing,8),(Scale,9),(Chord,11)] 
dorianScale =  [(Chord,0),(Scale,2),(Chord,3),(Passing,4),(Scale,5),
                (Chord,7),(Scale,9),(Chord,10)]
dominantScale =  [(Chord,0),(Scale,2),(Passing,3),(Chord,4),(Scale,5),(Passing,6),
                  (Chord,7),(Scale,9),(Chord,10),(Passing,11)]
alteredScale = [(Chord,0),(Scale,1),(Scale,3),(Chord,4),
                (Chord, 6),(Scale, 8),(Chord, 10)]
harmonicScale = [(Chord,0),(Scale,2),(Chord,3),(Scale,5),
                 (Chord,7),(Scale,9),(Passing,10),(Scale,11)]
locrianScale =  [(Chord,0),(Scale,1),(Chord,3),(Scale,5),(Chord,6),
                 (Scale,8),(Chord,10)]
                
