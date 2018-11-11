module Concerning where
    import Euterpea

    treblePinky1 :: [Music Pitch]
    treblePinky1 = [rest sn, b 4 en, b 4 sn, a 4 sn, (times 7 (e 5 en)), rest sn] ++
                  [times 8 (g 5 en), rest sn] ++
                  [times 8 (fs 5 en), rest sn] ++
                  [times 6 (e 5 en)]

    trebleThumb1 :: [Music Pitch]
    trebleThumb1 = [b 3 (sn + qn), (times 7 (e 4 en)), g 4 sn] ++
                  [g 4 (hn + hn), rest sn] ++
                  [times 8 (fs 4 en), e 4 sn] ++
                  [times 6 (e 4 en)]

    bassPinky1 :: [Music Pitch]
    bassPinky1 = [g 2 (sn + en), rest wn, e 3 sn] ++
                [e 3 (hn + hn), g 3 sn] ++
                [g 3 (hn + hn), c 3 sn] ++
                [times 6 (c 3 en)]

    bassThumb1 :: [Music Pitch]
    bassThumb1 = [rest sn, (d 3 en :=: g 3 en), d 3 sn, (a 3 (sn + hn + dqn) :=: (rest sn :+: times 7 (c 4 en))), rest sn] ++ 
                [times 8 ((b 3 en) :=: (e 4 en)), rest sn] ++
                [times 8 ((b 3 en) :=: (d 4 en)), rest sn] ++
                [times 6 ((g 3 en) :=: (c 4 en))]
            

    piano1 = times 3 ((line trebleThumb1) :=: (line treblePinky1) :=: (line bassPinky1) :=: (line bassThumb1))



    treblePinky2 :: [Music Pitch]
    treblePinky2 = [rest sn, b 4 en, b 4 sn, a 4 sn, (times 7 (e 5 en)), rest sn] ++
                  [times 8 (g 5 en), rest sn] ++
                  [times 8 (fs 5 en), rest sn] ++
                  [times 6 (e 5 en), rest sn]

    trebleThumb2 :: [Music Pitch]
    trebleThumb2 = [b 3 (sn + qn), (times 7 (e 4 en)), g 4 sn] ++
                  [g 4 (hn + hn), rest sn] ++
                  [times 8 (fs 4 en), e 4 sn] ++
                  [times 6 (e 4 en), b 3 sn]

    bassPinky2 :: [Music Pitch]
    bassPinky2 = [g 2 (sn + en), rest wn, e 3 sn] ++
                [e 3 (hn + hn), g 3 sn] ++
                [g 3 (hn + hn), c 3 sn] ++
                [times 6 (c 3 en), g 2 sn]

    bassThumb2 :: [Music Pitch]
    bassThumb2 = [rest sn, (d 3 en :=: g 3 en), d 3 sn, (a 3 (sn + hn + dqn) :=: (rest sn :+: times 7 (c 4 en))), rest sn] ++ 
                [times 8 ((b 3 en) :=: (e 4 en) :=: (g 3 en) :=: (d 4 en)), rest sn] ++
                [times 8 ((b 3 en) :=: (d 4 en)), rest sn] ++
                [times 6 ((g 3 en) :=: (c 4 en)), rest sn]
    

    piano2 = times 3 ((line trebleThumb2) :=: (line treblePinky2) :=: (line bassPinky2) :=: (line bassThumb2))
        

    concern = let t = (en / qn) * (132 / 120)
              in instrument MusicBox 
                            (tempo t (piano1 :+: piano2))