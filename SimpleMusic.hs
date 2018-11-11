 module SimpleMusic where
 import Euterpea

 data Inv = First | Second

 maj3 :: Pitch -> Pitch
 maj3 p = trans (4) p

 min3 :: Pitch -> Pitch
 min3 p = trans (3) p

 p5 :: Pitch -> Pitch
 p5 p = trans (7) p

 dim5 :: Pitch -> Pitch
 dim5 p = trans (6) p

 negP4 :: Pitch -> Pitch
 negP4 p = trans (-5) p

 major :: Dur -> Pitch -> Music Pitch
 major d p = note d p :=: note d (maj3 p) :=: note d (p5 p)

 minor :: Dur -> Pitch -> Music Pitch
 minor d p = note d p :=: note d (min3 p) :=: note d (p5 p)

 dimMinor :: Dur -> Pitch -> Music Pitch
 dimMinor d p = note d p :=: note d (min3 p) :=: note d (dim5 p)

 dimMinorInv :: Inv -> Dur -> Pitch -> Music Pitch
 dimMinorInv First d p = note d p :=: note d (trans (-6) p) :=: note d (trans (-9) p)
 dimMinorInv Second d p = note d p :=: note d (min3 p) :=: note d (dim5 p)

 majorInv :: Inv -> Dur -> Pitch -> Music Pitch
 majorInv First d p = note d p :=: note d (maj3 p) :=: note d (p5 p)
 majorInv Second d p = note d p :=: note d (maj3 p) :=: note d (negP4 p)

 minorInv :: Inv -> Dur -> Pitch -> Music Pitch
 minorInv First d p = note d p :=: note d (trans (5) p) :=: note d (trans (8) p)
 minorInv Second d p = note d p :=: note d (min3 p) :=: note d (negP4 p)

 sus4 :: Dur -> Pitch -> Music Pitch
 sus4 d p = note d p :=: note d (trans (5) p) :=: note d (trans (7) p)

 prog1 = major wn ((F, 4) :: Pitch) :+: minorInv Second hn ((A, 4) :: Pitch) :+: sus4 hn ((E, 4) :: Pitch)
 prog2 = major wn ((F, 4) :: Pitch) :+: minorInv Second ddqn ((A, 4) :: Pitch)
 prog3 = sus4 qn ((E, 4) :: Pitch) :+: minor qn ((E, 4) :: Pitch) :+: majorInv Second qn ((G, 4) :: Pitch) :+: dimMinorInv First qn ((B, 4) :: Pitch) :+: (minor wn ((D, 4) :: Pitch) :=: ((rest hn) :+: (a 5 qn) :+: (f 5 qn)))
 prog4 = (majorInv Second wn ((F, 4) :: Pitch)) :=: ((e 5 dhn) :+: (g 5 qn))
 prog5 = (minorInv Second wn ((A, 4) :: Pitch)) :=: ((e 5 wn))
 prog6 = (major hn ((C, 4) :: Pitch)) :=: ((c 5 hn)) :+: Modify (Tempo 1.6666667) ((major wn ((C, 4) :: Pitch)) :=: ((c 5 qn) :+: (d 5 en) :+: (e 5 qn) :+: (c 5 qn) :+: (d 5 en)))
 prog7 = (minor wn ((D, 4) :: Pitch)) :=: ((d 5 en) :+: (e 5 en) :+: (a 4 qn) :+: (a 5 qn) :+: (f 5 qn))
 
 rpt :: Int -> Music Pitch -> Music Pitch
 rpt 0 p = rest 0
 rpt times p = p :+: rpt (times-1) p

 transM :: AbsPitch -> Music Pitch -> Music Pitch
 transM ap (Prim (Note d p)) = note d (trans (ap) p)
 transM ap (Prim (Rest d)) = rest d
 transM ap (m1 :+: m2) = transM ap m1 :+: transM ap m2
 transM ap (m1 :=: m2) = transM ap m1 :=: transM ap m2
 transM ap (Modify cntrl m) = Modify cntrl (transM ap m)
 
 myMel8 = Modify (Tempo 0.5) (prog1 :+: prog2 :+: prog3 :+: prog4 :+: prog5 :+: prog6 :+: prog7)
 modMyMel8 = Modify (Instrument Pad2Warm) (myMel8)
 modMyMel8Trans = transM (-12) (Modify (Instrument FX6Goblins) myMel8)

 fuck = (modMyMel8 :=: modMyMel8Trans)