module Rand where

import Data.List
import Control.Monad.State
import System.Random
import Debug.Trace

class Randable r where
  getGen ::  r -> StdGen
  setGen ::  StdGen -> r -> r
  
getRandom :: (Random a, Randable r) => State r a 
getRandom = do
  st <- get
  let (res,gen') = random $ getGen st
  put $ setGen gen' st
  return res
  
runRandom :: (Randable r) => State r a -> StdGen -> r -> a
runRandom st g r = evalState st $ setGen g r

runRandomIO :: (Randable r) => State r a -> r -> IO a
runRandomIO st r = do
  g <- randomIO :: IO Int
  return $ runRandom st (mkStdGen g) r
  
type Rand = State StdGen

instance Randable StdGen where
  getGen = id
  setGen = const
  
select :: (Randable r) => [(Double,a)] -> State r a
select selections = do
  let probsum = sum $ map fst selections
      selections' = map (\(a,b) -> (a/probsum,b)) $ scanl1 (\(acc_prob,_) (prob,x) -> (acc_prob+prob,x)) selections
  rvar <- getRandom 
  case find ((rvar<).fst) selections' of
    (Just (_,result)) -> return result
    Nothing -> error $ "selection: everybody is going to die"