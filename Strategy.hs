module Strategy where

import Chord
import Note
import Rand
import Utils

-- a strategy is... a function from an in-progression chord to a list of notes.  
type Strategy = Modality -> Int -> Rand [(Length,NoteValue)]

arpeggioRandom :: (Double,Double,Double,Double) -> Strategy
arpeggioRandom (rootprob,thirdprob,fifthprob,seventhprob) mode n = do 
  let chordnotes = absoluteArpeggio mode
  notes <- sequence $ replicate (n-1) $  select $ zip [rootprob,thirdprob,fifthprob,seventhprob] chordnotes
  return $ (map ((,)Quarter) notes) ++ [(Quarter,head chordnotes)]
  
arpeggioUp :: Strategy
arpeggioUp mode n = do
  let chordnotes = absoluteArpeggio mode
  return $ (map ((,)Quarter) $ reverse $ take n $ cycle chordnotes)
  
arpeggioDown :: Strategy
arpeggioDown mode n = do
  let chordnotes = absoluteArpeggio mode
  return $ (map ((,)Quarter) $ reverse $ take n $ cycle $ (headReverse chordnotes))
    
scaleUp :: Double -> Strategy
scaleUp passingProb mode n = do 
  let scale = absoluteScale mode
  result <- filterMUntil n (scaleProbFilter passingProb) $ cycle $ scale
  return  $ map ((,)Quarter) $ reverse $ map snd result
  
scaleDown :: Double -> Strategy
scaleDown passingProb mode n = do 
  let scale = absoluteScale mode
  result <- filterMUntil n (scaleProbFilter passingProb) $ cycle $ headReverse scale
  return $ map ((,)Quarter) $ reverse $ map snd result

hang :: Strategy
hang mode n = return  $ replicate n (Quarter,modeRoot mode)



scaleProbFilter passingProb (Passing,c) = do
  prob <- getRandom
  return $ prob < passingProb 
scaleProbFilter _ res = return True



-- a strategy adapter takes valid output from a strategy and adapts it to be less lame
-- this -> targetChord -> stratgyOutput -> adaptedStrategyOutput
type StrategyAdapter = Modality -> Modality -> [(Length,NoteValue)] -> Rand [(Length,NoteValue)]

identityStrategyAdapter :: StrategyAdapter
identityStrategyAdapter _ _ res = return res

resolveQuarter :: StrategyAdapter
resolveQuarter _ _ [one_note] = return [one_note]
resolveQuarter _ target ((Quarter,c):rst) = do
  b <- getRandom :: Rand Bool
  let succOrPred = if b then succNote else predNote
  return $ (Quarter,succOrPred $ modeRoot target):rst
resolveQuarter _ _ any = return any
  
resolveEighth :: StrategyAdapter
resolveEighth _ target ((Quarter,c):rst) = do
  succOrPred <- fmap (\b -> if b then succNote else predNote) getRandom
  return $ (Eighth,succOrPred $ modeRoot target):(Eighth,c):rst
resolveEighth _ _ any = return any


numRepeats = 10

addPassingTone :: Double -> Double -> StrategyAdapter
addPassingTone probability probInDir modality target notes = do
  probs <- mapM (const getRandom) notes
  probs2 <- mapM (const getRandom) notes
  return $ concat $ tail $ scanl
    (\nexts ((ps,prob),(len,nt)) ->
      let (_,nextNote) = last nexts in
      if len == Quarter && not ((intervalBetween nextNote nt) `elem` [0,1,fromIntegral $ numNotes-1]) && prob < probability 
      then [(Eighth, if ps < probInDir && (isAscending nextNote nt)
                     then succNote nextNote else predNote nextNote),
            (Eighth,nt)]
      else [(len,nt)])
    [(undefined,modeRoot target)]
    (zip (zip probs2 probs) notes)

addDouble :: Double -> StrategyAdapter
addDouble prob _ _ notes = 
  fmap concat $ mapM (\(len,c) -> getRandom >>= 
                                  (\r -> if len == Quarter && r < prob
                                         then return [(Eighth,c),(Eighth,c)]
                                         else return [(len,c)]))
    notes
    
