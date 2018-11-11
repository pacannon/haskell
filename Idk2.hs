module Idk where
    import Euterpea

    octaves :: [Int] -> Music a -> Music a
    octaves [] mus = rest 0
    octaves (o:os) mus = (transpose (12 * o) mus) :=: (octaves os mus)

    part :: [Pitch] -> [Dur] -> Dur -> Music Pitch
    part (n:ns) (d:ds) dur = let rem = dur - d
                             in if rem <= 0
                                then (note dur n)
                                else ((note d n) :+: (part (ns ++ [n]) (ds ++ [d]) (dur - d)))

    chordProgression = [4, 1, 6]

    rootToChord :: Int -> [Pitch]
    rootToChord 0 = [(C, 4), (E, 4), (G, 4)]
    rootToChord 1 = [(D, 4), (F, 4), (A, 4)]
    rootToChord 2 = [(E, 4), (G, 4), (B, 4)]
    rootToChord 3 = [(F, 4), (A, 4), (C, 4)]
    rootToChord 4 = [(G, 4), (B, 4), (D, 5)]
    rootToChord 5 = [(A, 4), (C, 5), (E, 5)]
    rootToChord 6 = [(B, 4), (D, 5), (F, 5)]
    rootToChord 7 = [(C, 5), (E, 5), (G, 5)]

    phrase = [0, 12, 7, 5, 4, 3, (6 / 7), (7 / 8)]


{-    data BluesPitchClass = Ro | MT | Fo | Fi | MS
    type BluesPitch = (BluesPitchClass, Octave) -}

    type Root = Pitch

    data Song = Song { melody :: [Int] , rhythm :: [Int] , root :: Root , rootDur :: Dur }

    aSong :: Song
    aSong = Song { melody = [0, 5, 2, -2], rhythm = [1, 1, 1, 1],
                   root = (C, 4), rootDur = qn }
                   
    bSong :: Song
    bSong = Song { melody = [0, 5, 2, -2], rhythm = [1, 1, 1, 1],
                    root = (G, 4), rootDur = qn }
    
    cSong :: Song
    cSong = Song { melody = [0, 5, 2, -2], rhythm = [1, 1, 1, 1],
                    root = (F, 4), rootDur = qn }

    dSong :: Song
    dSong = Song { melody = [0, 0, 2, -2], rhythm = [1, 1, 1, 1],
                root = (E, 4), rootDur = qn }
    
    toPlay :: Song -> Music Pitch
    toPlay (Song { melody = melody, rhythm = rhythm, root = root, rootDur = rootDur }) = measure root rootDur melody rhythm

    measureD :: Pitch -> Dur -> Int -> Dur -> Music Pitch
    measureD root rd translate dur = note (wn * rd * dur) (pitch ((absPitch root) + translate))

    measure :: Pitch -> Dur -> [Int] -> [Int] -> Music Pitch
    measure root rd [p] [d] = note (wn * rd * (toRational d)) (pitch ((absPitch root) + p))
    measure root rd (p:ps) (d:ds) = (measure root rd [p] [d]) :+: (measure (pitch (absPitch(root) + p)) rd ps ds)

    mergeSongs :: Song -> Song -> Music Pitch
    mergeSongs songA songB = (toPlay songA) :=: (toPlay songB)

    concatSongs :: Song -> Song -> Music Pitch
    concatSongs (Song { melody = melodyA, rhythm = rhythmA, root = rootA, rootDur = rootDurA }) (Song { melody = melodyB, rhythm = rhythmB, root = rootB, rootDur = rootDurB }) = (toPlay (Song { melody = melodyA, rhythm = rhythmA, root = rootA, rootDur = rootDurA })) :+: (toPlay (Song { melody = melodyB, rhythm = rhythmB, root = (pitch ((absPitch rootB) + (foldl (+) 0 melodyA))), rootDur = rootDurB }))

    theSong = (concatSongs () ())

