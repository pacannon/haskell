module Chapter4 where
    import Euterpea

    prefixes :: [a] -> [[a]]
    prefixes [] = []
    prefixes (x:xs) = let f pf = x : pf
                      in [x] : map f (prefixes xs)

    prefix :: [Music a] -> Music a
    prefix mel = let m1 = line (concat (prefixes mel))
                     m2 = transpose 12 (line (concat (prefixes (reverse mel))))
                     m = instrument BaritoneSax m1 :=: instrument Oboe m2
                 in m :+: transpose 5 m :+: transpose 10 m :+: transpose (-9) m
    
    mel1 = [c 5 en, e 5 sn, g 5 en, b 5 sn, a 5 en, f 5 sn, d 5 en, b 4 sn, c 5 en ]
    mel2 = [c 5 sn, e 5 sn, g 5 sn, b 5 sn, a 5 sn, f 5 sn, d 5 sn, b 4 sn, c 5 sn ]

    myMel = [a 2 en, a 2 en, c 3 en, e 3 en, f 3 qn, d 3 en, e 2 en]