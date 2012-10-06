module Render where


import qualified Haskore.Melody as HM
import qualified Haskore.Basic.Duration as HD
import qualified Haskore.Music as HMu
import qualified Haskore.Music.GeneralMIDI as MidiMusic
import qualified Haskore.Interface.MIDI.Render as Render


import Note

renderNote :: (Length,Note) -> HM.T ()
renderNote (l,Note cn n) = tnote n (d l) ()
  where d Quarter = HD.qn
        d Eighth = HD.en
        d Whole = HD.wn
        tnote = case cn of
          C -> HM.c
          Db -> HM.df
          D -> HM.d
          Eb -> HM.ef
          E -> HM.e
          F -> HM.f
          Gb -> HM.gf
          G -> HM.g
          Ab -> HM.af
          A -> HM.a
          Bb -> HM.bf
          B -> HM.b
          
renderNotes :: [(Length,Note)] -> HM.T ()
renderNotes nts = foldl1 (HMu.+:+) $ map renderNote nts

renderNotesToFile :: FilePath -> [(Length,Note)] -> IO ()
renderNotesToFile fl nts = render_to fl (renderNotes nts)
  
  
-- copied from tutorial
render_to f m = Render.fileFromGeneralMIDIMusic f song where
  song = MidiMusic.fromMelodyNullAttr MidiMusic.AcousticBass m