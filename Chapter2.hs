module Chapter2 where
    import Euterpea
   
    data Inv = First | Second
   
    maj3 :: Pitch -> Pitch
    maj3 p = trans (4) p
   
    min3 :: Pitch -> Pitch
    min3 p = trans (3) p
   
    maj2 :: Pitch -> Pitch
    maj2 p = trans (2) p
   
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
   
    twoFiveOne :: Pitch -> Dur -> Music Pitch
    twoFiveOne p d = (minor d (maj2 p)) :+: (major d (p5 p)) :+: (major (2*d) p)

    data BluesPitchClass = Ro | MT | Fo | Fi | MS
    type BluesPitch = (BluesPitchClass, Octave)

    ro, mt, fo, fi, ms :: Octave -> Dur -> Music BluesPitch
    ro o d = note d (Ro, o)
    mt o d = note d (MT, o)
    fo o d = note d (Fo, o)
    fi o d = note d (Fi, o)
    ms o d = note d (MS, o)

    fromBlues :: Music BluesPitch -> Music Pitch
    fromBlues (Prim (Note d (Ro, o))) = (note d (C, o))
    fromBlues (Prim (Note d (MT, o))) = (note d (Ef, o))
    fromBlues (Prim (Note d (Fo, o))) = (note d (F, o))
    fromBlues (Prim (Note d (Fi, o))) = (note d (G, o))
    fromBlues (Prim (Note d (MS, o))) = (note d (Bf, o))
    fromBlues (Prim (Rest d)) = (rest d)
    fromBlues (m1 :+: m2) = fromBlues m1 :+: fromBlues m2
    fromBlues (m1 :=: m2) = fromBlues m1 :=: fromBlues m2
    fromBlues (Modify ctrl (m)) = Modify ctrl (fromBlues m)

    mel1, mel2 :: Music BluesPitch
    mel1 = note qn (Ro, 4) :+: note qn (MT, 4) :+: note hn (Fi, 4)
    mel2 = note en (Ro, 4) :+: note qn (MT, 4) :+: note en (Fi, 4) :+: note qn (Fo, 4) :+: note en (MS, 4) :+: note en (Fo, 4)

