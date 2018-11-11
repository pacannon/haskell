module Chapter6 where
    import Euterpea

    octaves :: [Int] -> Music a -> Music a
    octaves [] mus = rest 0
    octaves (o:os) mus = (transpose (12 * o) mus) :=: (octaves os mus)

    toProg :: [Int] -> Pitch -> Mode -> Music Pitch
    toProg [] tonic mode = rest 0
    toProg (p:ps) tonic mode = (note (qn/3) (degreeToPitch tonic mode p)) :=: (toProg ps tonic mode)

    tooProg :: [Int] -> Pitch -> Mode -> [Int] -> Music Pitch
    tooProg prog tonic mode [] = rest 0
    tooProg prog tonic mode (p:ps) = (toProg prog (pitch ((degreeToInterval mode p) + (absPitch tonic))) mode) :+: (tooProg prog tonic mode ps)

    type Scale = [Int]

    modeToScale :: Mode -> Scale
    modeToScale Lydian = [0, 2, 4, 6, 7, 9, 11]
    modeToScale Ionian = [0, 2, 4, 5, 7, 9, 11]
    modeToScale Mixolydian = [0, 2, 4, 5, 7, 9, 10]
    modeToScale Dorian = [0, 2, 3, 5, 7, 9, 10]
    modeToScale Aeolian = [0, 2, 3, 5, 7, 8, 10]
    modeToScale Phrygian = [0, 1, 3, 5, 7, 8, 10]
    modeToScale Locrian = [0, 1, 3, 5, 6, 8, 10]

    degreeToInterval :: Mode -> Int -> Int
    degreeToInterval mode deg = let transD = (deg - 1) `mod` 7
                                    scale = modeToScale mode
                                in ((12 * ((deg - 1) `div` 7))) + (scale !! transD)

    degreeToPitch :: Pitch -> Mode -> Int -> Pitch
    degreeToPitch tonic mode degree = pitch ((degreeToInterval mode degree) + (absPitch (tonic)))

    songA1 = instrument Vibraphone (octaves [-1, 1] (tooProg ([1, 3, 5]) (D, 3) Dorian ([1, 3, 5])))
    songA2 = instrument StringEnsemble1 (octaves [0] (tooProg ([5]) (D, 3) Dorian ([5, 3, 5])))
    songA3 = instrument BaritoneSax (octaves [-1] (tooProg ([3]) (D, 3) Dorian ([3, 1, 5])))
    songA4 = instrument SynthBass1 (octaves [-2] (tooProg ([1]) (D, 3) Dorian ([1])))

    songB1 = instrument Vibraphone (octaves [-1, 1] (tooProg ([1, 3, 5]) (C, 4) Ionian ([1, 3, 5])))
    songB2 = instrument StringEnsemble1 (octaves [0] (tooProg ([5]) (C, 4) Ionian ([5, 3, 5])))
    songB3 = instrument BaritoneSax (octaves [-1] (tooProg ([3]) (C, 4) Ionian ([3, 1, 5])))
    songB4 = instrument SynthBass1 (octaves [-2] (tooProg ([1]) (C, 4) Ionian ([1])))

    songC1 = instrument Vibraphone (octaves [-1, 1] (tooProg ([1, 3, 5]) (E, 4) Phrygian ([1, 3, 5])))
    songC2 = instrument StringEnsemble1 (octaves [0] (tooProg ([5]) (E, 4) Phrygian ([5, 3, 5])))
    songC3 = instrument BaritoneSax (octaves [-1] (tooProg ([3]) (E, 4) Phrygian ([3, 1, 5])))
    songC4 = instrument SynthBass1 (octaves [-2] (tooProg ([1]) (E, 4) Phrygian ([1])))

    phrasePart :: Pitch -> Mode -> Music Pitch
    phrasePart tonic mode = let song1 = instrument Vibraphone (octaves [-1, 1] (tooProg ([1, 3, 5]) tonic mode ([1, 3, 5])))
                                song2 = instrument StringEnsemble1 (octaves [0] (tooProg ([5]) tonic mode ([5, 3, 5])))
                                song3 = instrument BaritoneSax (octaves [-1] (tooProg ([3]) tonic mode ([3, 1, 5])))
                                song4 = instrument SynthBass1 (octaves [-2] (tooProg ([1]) tonic mode ([1])))
                            in song1 :=: song2 :=: song3 :=: song4

    song = times 2 (
            (times 4 (phrasePart (D, 3) Dorian)) :+:
            (times 4 (phrasePart (C, 3) Ionian)) :+:
            (times 4 (phrasePart (E, 3) Phrygian))) :+:
           times 2 (
            (times 4 (phrasePart (A, 3) Aeolian)) :+:
            (times 4 (phrasePart (G, 3) Mixolydian)) :+:
            (times 4 (phrasePart (E, 3) Phrygian))) :+: 
            (phrasePart (F, 3) Lydian) :+:
            (phrasePart (C, 4) Ionian) :+:
            (phrasePart (G, 4) Mixolydian) :+:
            (phrasePart (D, 3) Dorian) :+:
            (phrasePart (A, 3) Aeolian) :+:
            (phrasePart (E, 3) Phrygian) :+:
            (phrasePart (B, 4) Locrian)