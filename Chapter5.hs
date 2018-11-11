module Chapter4 where
    import Euterpea

    twice :: (a -> a) -> a -> a
    twice f = f . f

    power :: (a -> a) -> Int -> a -> a
    power f 0 arg = arg
    power f n arg = f (power f (n-1) arg)

    apPairs :: [AbsPitch] -> [AbsPitch] -> [(AbsPitch, AbsPitch)]
    apPairs aps1 aps2 = [(a1+45, a2+45) | a1 <- aps1, a2 <- aps2, abs (a1 - a2) > 2, abs (a1 - a2) < 8]

    -- conv :: [(AbsPitch, AbsPitch)] -> Music Pitch
    -- conv ps = foldl (:+:) (map (:=:) ps) (rest 0)


    dog :: (AbsPitch, AbsPitch) -> Music Pitch
    dog (a, b) = let dur = if even a then sn else en
                 in (note dur (pitch a)) :=: (note dur (pitch b))

    addDur :: Dur -> [Dur -> Music a] -> Music a
    addDur d ns = let f n = n d
                  in line (map f ns)

   {- dogs :: [(AbsPitch, AbsPitch)] -> ([Dur -> Music Pitch])
    dogs = map dog-}
    -- cats = map (qn) dogs

    freak :: [(a -> b)] -> a -> [b]
    freak [] arg = []
    freak (fn:fns) arg = (fn arg) : (freak fns arg)

    -- cats = freak qn dogs
 --   cat :: [(AbsPitch, AbsPitch)] -> Music Pitch
--    cat ps = foldl (:+:) ((map dog ps) qn) (rest 0)

    mPitches = map dog (apPairs [1, 2, 3, 5, 8, 13, 8, 5, 3, 2, 1, 1] [1, 2, 3, 5, 8, 13, 8, 5, 3, 2, 1, 1])

    aPa :: [(AbsPitch, AbsPitch)]
    aPa = (apPairs [1..20] [8..27])

    mps :: [Music Pitch]
    mps = map dog aPa

    ohNo :: Music Pitch
    ohNo = line mps

    -- hList d = line . map (hNote d)
