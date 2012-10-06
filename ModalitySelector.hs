module ModalitySelector where

import Note
import Chord
import Rand

type ModalityRule = [(Int,Modality)] -> Rand (Maybe [(Int,Modality)])

tritoneSub :: ModalityRule
tritoneSub (two:five:rst) 
  | and [modeType (snd two) `elem` [Dorian,Locrian],
         modeType (snd five) `elem` [Dominant,Altered],
         intervalBetween (modeRoot $ snd two) (modeRoot $ snd five) `elem` [Fourth,MajorSeventh]] 
    = getRandom >>= (\b -> return $ Just $ [two,(fst five,altered $ (if b then Tritone else Root) `above` (modeRoot $ snd five))]++rst)
tritoneSub _ = return Nothing

addTwoFive :: ModalityRule
addTwoFive (nottwo:(n,five):one:rst)
  | and [even n,
         modeType five `elem` [Dominant,Altered],
         intervalBetween (modeRoot $ five) (modeRoot $ snd one) `elem` [Fourth,MajorSeventh],
         intervalBetween (modeRoot $ snd nottwo) (modeRoot $ five) `notElem` [Fourth,MajorSeventh]] 
    = return $ Just $ [nottwo,(n`div`2, dorian $ Fourth `below` (modeRoot five)),(n`div`2,five),one] ++ rst

