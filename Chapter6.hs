module Chapter6 where
    import Euterpea

    s1 :: [Int]
    s1 = [1,5,3,6,5,0,1,1];
    {-s2 :: [Int]
    s2 = [4, (−2), 3, (−1), (−5), 1, 0];
    -}

    {-
        a) Define a function toIntervals that takes a list of n numbers, and generates
        a list of n lists, such that the ith list is the sequence si as defined above.
    -}

    differentiate :: [Int] -> [Int]
    differentiate (a:b:[]) = [b - a]
    differentiate (a:b:cs) = (b - a) : differentiate (b:cs)

    toIntervals :: [Int] -> [[Int]]
    toIntervals (n:[]) = []
    toIntervals (n:ns) = let diff = (differentiate (n:ns))
                         in [diff] ++ toIntervals diff

    {-
        b) Define a function getHeads that takes a list of n lists and returns a list
        of n numbers such that the ith element is the head of the ith list.
    -}

    getHeads :: [[Int]] -> [Int]
    getHeads [] = []
    getHeads ((h:hs):ls) = [h] ++ getHeads ls

    intervalClosure :: [Int] -> [Int]
    intervalClosure l = reverse (getHeads (toIntervals l))

    {-
    funkGroove :: Music Pitch
    funkGroove = let p1 = perc LowTom qn
                     p2 = perc AcousticSnare en
                 in tempo 3 $ instrument Percussion $ takeM 8 $ repeatM ((p1 :+: qnr :+: p2 :+: qnr :+: p2 :+:
                    p1 :+: p1 :+: qnr :+: p2 :+: enr) :=: roll en (perc ClosedHiHat 2))
-}
    kl = line [(c 4 qn), (e 4 en), (g 4 en), (c 5 qn), (e 5 en), (g 5 en)];
    k = lineToList (kl);

    invertRetro :: Music Pitch -> Music Pitch
    invertRetro = invert . retro

    kly = (kl :+: (invert kl) :+: (retro kl) :+: (retroInvert kl))

    {-
    invert :: Music Pitch -> Music Pitch
    invert m =
        let l@(Prim (Note _ r) : _ ) = lineToList m
            inv (Prim (Note d p)) =
                note d (pitch (2 * absPitch r - absPitch p))
            inv (Prim (Rest d)) = rest d
        in line (map inv l)
        -}