module Main where

import SongParser
import Render
import Rand
import Strategies.Standard
import BassLineGenerator
import Debug.Trace
import Chord
import Note

main = do
  modes <- parseSongFromFile "Songs/BluesForAlice.sng"
  result <- runRandomIO (generateBassLineFromModalities strategy strategyAdapter modes) undefined
  renderNotesToFile "output.midi" (trace (show result) result)
  
