module ComplexMusic where
    import Euterpea

    maj3 :: Pitch -> Pitch
    maj3 p = trans (4) p
   
    min3 :: Pitch -> Pitch
    min3 p = trans (3) p
   
    p5 :: Pitch -> Pitch
    p5 p = trans (7) p

    dim5 :: Pitch -> Pitch
    dim5 p = trans (6) p
   
    major :: Dur -> Pitch -> Music Pitch
    major d p = note d p :=: note d (maj3 p) :=: note d (p5 p)
   
    minor :: Dur -> Pitch -> Music Pitch
    minor d p = note d p :=: note d (min3 p) :=: note d (p5 p)
   
    dimMinor :: Dur -> Pitch -> Music Pitch
    dimMinor d p = note d p :=: note d (min3 p) :=: note d (dim5 p)
    
    song :: Integer -> Rational -> Music Pitch
    song bpm timeSig = let tempO = ((fromIntegral bpm) / 120)
                       in tempo (tempO) (phraser 0 wn [(5, 1/2), (2, 1)])

    phraser :: Int -> Dur -> [(Int, Rational)] -> Music Pitch
    phraser root dur [] = note dur (pitch (root + (absPitch (C, 4))))
    phraser root dur ((dp, dd):ds) = note dur (pitch (root + (absPitch (C, 4)))) :+: phraser (root + dp) (dur * dd) ds


    rootsToChords :: [(Int, Dur)] -> Music Pitch
    rootsToChords [] = rest 0
    rootsToChords ((r, dur):rs) = (rootToChord r dur) :+: (rootsToChords rs)

    rootToChord :: Int -> Dur -> Music Pitch
    rootToChord 1 dur = major dur (C, 4)
    rootToChord 2 dur = minor dur (D, 4)
    rootToChord 3 dur = minor dur (E, 4)
    rootToChord 4 dur = major dur (F, 4)
    rootToChord 5 dur = major dur (G, 4)
    rootToChord 6 dur = minor dur (A, 4)
    rootToChord 7 dur = dimMinor dur (B, 4)