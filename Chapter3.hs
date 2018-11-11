module Chapter2 where
    import Euterpea
   
    data Inv = First | Second

    concertA, a440 :: (PitchClass, Octave)
    concertA = (A, 4) -- concert A
    a440 = (A, 4) -- A440
   
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

    wts :: Pitch -> [Music Pitch]
    wts p = let f ap = note qn (pitch (absPitch p + ap))
            in map f [0, 2, 4, 6, 8]


    f11 :: Int -> Pitch -> Pitch   
    f11 i p = trans (i) p

    f1 :: Int -> [Pitch] -> [Pitch]
    f1 i ps = map (f11 i) ps 

    f22 :: Dur -> Music a
    f22 dur = rest dur

    f2 :: [Dur] -> [Music a]
    f2 durs = map f22 durs


    f33 :: Music Pitch -> Music Pitch
    f33 (Prim (Note d p)) = note (d/2) p :+: rest (d/2)
    f33 (Prim (Rest d)) = rest d

    f3 :: [Music Pitch] -> [Music Pitch]
    f3 mps = map f33 mps

    combine :: [Music Pitch] -> Music Pitch
    combine [] = rest 0
    combine (p:ps) = p :+: combine ps

    simple :: Int -> Int -> Int -> Int
    simple a b c = a * (b + c)

    applyEach :: [a -> b] -> a -> [b]
    applyEach [] init = []
    applyEach (fn:fns) init = ((fn init) : (applyEach fns init))

    applyAll :: [a -> a] -> a -> a
    applyAll [] init = init
    applyAll (fn:fns) init = fn (applyAll fns init)

    toUnit :: a -> Int
    toUnit thing = 1

    mapToUnit :: [a] -> [Int]
    mapToUnit ls = map toUnit ls

    nonRecLength :: [a] -> Int
    nonRecLength ls = foldr (+) 0 (mapToUnit ls)

    doubleEach :: [Int] -> [Int]
    doubleEach ints = map (*2) ints

    toTuple :: Int -> (Int, Int)
    toTuple x = (x, x+1)

    pairAndOne :: [Int] -> [(Int, Int)]
    pairAndOne ints = map toTuple ints

    addEachPair :: [(Int, Int)] -> [Int]
    addEachPair p = let f (a, b) = a+b
                    in map f p

    addPairsPointwise :: [(Int, Int)] -> (Int, Int)
    addPairsPointwise [] = (0,0)
    addPairsPointwise ((a,b):ps) = let (x,y) = addPairsPointwise ps
                                   in (a+x,b+y)

    fuse :: [Dur] -> [Dur -> Music a] -> [Music a]
    fuse [] [] = []
    fuse (d:ds) (p:ps) = ((p d):(fuse ds ps))


    -- Frere-Jaques using some more Euterpea features

    fj0, fj1, fj2, fj3, fj4 :: Music Pitch
    fj0 = c 4 qn :+: c 4 qn :+: c 4 qn
    fj1 = c 4 qn :+: d 4 qn :+: e 4 qn :+: c 4 qn
    fj2 = e 4 qn :+: f 4 qn :+: g 4 hn
    fj3 = g 4 en :+: a 4 en :+: g 4 en :+: f 4 en :+: e 4 qn :+: c 4 qn
    fj4 = c 4 qn :+: g 3 qn :+: c 4 hn

    fj :: Music Pitch
    fj  = two fj1 :+: two fj2 :+: two fj3 :+: two fj4
        where two m = m :+: m

    fjfj :: Music Pitch
    fjfj = Modify (Tempo 1) (Modify (Instrument AcousticGrandPiano) fj)





    toTuple2 :: Music Pitch -> (Music Pitch, Music Pitch, Music Pitch, Music Pitch)
    toTuple2 x = let rest1 = (rest wn) :+: (rest wn)
                 in (x, rest1 :+: x, rest1 :+: rest1 :+: x, rest1 :+: rest1 :+: rest1 :+: x)
    pairAndOne2 :: Music Pitch -> (Music Pitch, Music Pitch, Music Pitch, Music Pitch)
    pairAndOne2 ms = toTuple2 ms

    doIt = let (fj01, fj02, fj03, fj04) = pairAndOne2 fj
           in Modify (Tempo 1) (Modify (Instrument AcousticGrandPiano) fj01) :=: Modify (Tempo 1) (Modify (Instrument Viola) fj02) :=: Modify (Tempo 1) (Modify (Instrument Oboe) fj03) :=: Modify (Tempo 1) (Modify (Instrument Trumpet) fj04)