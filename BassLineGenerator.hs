module BassLineGenerator where

import Chord
import Note
import Strategy
import Control.Monad
import Rand

generateBassLineFromModalities :: Strategy -> StrategyAdapter -> [(Int,Modality)] -> Rand [(Length,Note)]
generateBassLineFromModalities strat stratApt modes = do
  notes <- fmap (concat.(map reverse).(++[[(Whole,modeRoot $ snd $ head modes)]])) $ 
             mapM (\((n,mode),nextMode) -> strat mode n >>= (\res -> stratApt mode nextMode res)) $ zip modes $ tail $ cycle $ map snd modes
  return $ zip (map fst notes) (noteValuesToLine $ map snd notes)
  
startingRange = 2
  
noteValuesToLine :: [NoteValue] -> [Note]
noteValuesToLine notes = scanl nearestNote (Note (head notes) startingRange) $ tail notes
  
