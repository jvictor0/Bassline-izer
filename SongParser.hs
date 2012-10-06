module SongParser where

import Chord
import Utils
import Data.List
import System.IO


parseSong :: String -> [(Int,Modality)]
parseSong str = p' $ group $ words $ filter (/='|') str
  where p' [] = []
        p' ([chrd]:("/":sls):rsts) = (2+(length sls),unShow chrd):(p' rsts)
        p' ([chrd]:rst) = (1,unShow chrd):(p' rst)
        
-- for now, errors are just gonna be kinda shitty, which is fine.  
parseSongFromFile :: FilePath -> IO [(Int,Modality)]
parseSongFromFile filename = do
  catch (fmap parseSong $ readFile filename)
    (error "Parse Error on file")
  
