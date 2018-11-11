module Opening where
    import Euterpea

    t1 = times 6 (f 4 en :+: af 4 en)
    t2 = times 6 (c 5 en :+: f 4 en)
    t3 = times 6 (g 4 en :+: c 5 en)
    t4 = (times 5 (af 4 en :+: c 5 en)) :+: (g 4 en :+: c 5 en)

    tA00 = t1 :+: t2 :+: t3 :+: t4
    tA01 = t1 :+: t2 :+: t3 :+: t3

    tA = tempo (180 / 120) ((times 3 tA00) :+: tA01)

    bh1 = times 8 (af 3 en :+: c 4 en) :+: times 8 (g 3 en :+: bf 3 en)
    bl1 = tempo (1 / 2) (f 3 wn) :+: ef 3 wn :+: df 3 wn
    bl2 = tempo (1 / 2) (f 3 wn) :+: ef 3 wn :+: bf 2 wn

    bA00 = bh1 :=: bl1
    bA01 = bh1 :=: bl2

    bA = times 3 (bA00) :+: bA01

    sectionA = tA :=: bA

    -- B
    b_t_0_0 = times 6 (c 4 en :+: f 4 en)
    b_t_0_1 = times 6 (d 4 en :+: f 4 en)
    b_t_0_2 = times 6 (bf 3 en :+: ef 4 en)
    b_t_0_3 = times 6 (c 4 en :+: ef 4 en)

    b_t_0 = b_t_0_0 :+: b_t_0_1 :+: b_t_0_2 :+: b_t_0_3
    b_t_1 = b_t_0_0 :+: b_t_0_1 :+: b_t_0_2 :+: b_t_0_2





    b_t = tempo (180 / 120) (times 3 (b_t_0) :+: b_t_1)


    b_b_h_0 = times 8 (f 3 en :+: af 3 en) :+: times 8 (ef 3 en :+: g 3 en)


    b_b_l_0 = c 3 wn :+: bf 2 wn :+: bf 2 wn :+: g 2 wn

    b_b_0 = b_b_h_0 :=: b_b_l_0



    b_b = times 4 (b_b_0)

    sectionB = b_t :=: b_b


    -- C

    c_t_0_0 = times 6 (d 5 en :+: f 4 en)
    c_t_0_1 = times 6 (af 4 en :+: d 5 en)
    c_t_0_2 = times 6 (ef 5 en :+: af 4 en)


    c_t_1_2 = times 6 (d 5 en :+: af 4 en)
    c_t_1_3 = times 6 (d 5 en :+: bf 4 en)

    c_t_0 = c_t_0_0 :+: c_t_0_1 :+: c_t_0_2 :+: c_t_0_2
    c_t_1 = c_t_0_0 :+: c_t_0_0 :+: c_t_1_2 :+: c_t_1_3

    c_t = tempo (180 / 120) (times 3 (c_t_0) :+: c_t_1)

    c_b_h_0 = times 8 (bf 3 en :+: d 4 en) :+: times 8 (c 4 en :+: ef 4 en)
    c_b_h_1 = times 16 (bf 3 en :+: d 4 en)

    c_b_h = times 3 (c_b_h_0) :+: c_b_h_1

    c_b_l_0 = tempo (1 / 2) (f 3 wn) :+: af 3 wn :+: bf 3 wn
    c_b_l_1 = tempo (1 / 2) (f 3 wn) :+: af 3 wn :+: bf 3 hn :+: af 3 hn
    c_b_l_2 = tempo (1 / 2) (g 3 wn) :+: tempo (1 / 2) (f 3 wn)

    c_b_l = times 2 (c_b_l_0) :+: c_b_l_1 :+: c_b_l_2

    c_b = c_b_h :=: c_b_l

    sectionC = c_t :=: c_b

    partA = times 3 (sectionA :+: sectionB :+: sectionC)

    bt = tempo (92 / 120) (partA)

    modBt = Modify (Instrument AcousticGrandPiano) (bt)

    okay :: Int -> Dur -> (Pitch, Pitch) -> Music Pitch
    okay 1 dur (p1,p2) = note dur p1
    okay rem dur (p1,p2) = (note dur p1) :+: (okay (rem-1) dur (p2,p1))

    dance :: Dur -> Int -> Int -> (Pitch, Pitch) -> (Pitch, Pitch) -> Music Pitch
    dance dur num denom bassTup trebTup = (okay (num) (dur / (fromIntegral num)) trebTup) :=: (okay (denom) (dur / (fromIntegral denom)) bassTup)

    magic :: Music Pitch
    magic = dance wn 7 4 (((Af, 3) :: Pitch), ((C, 4) :: Pitch)) (((F, 4) :: Pitch), ((Af, 4) :: Pitch))
    
    magicArg :: Dur -> Int -> Int -> (Pitch, Pitch) -> (Pitch, Pitch) -> Music Pitch
    magicArg dur num denom (b1, b2) (t1, t2) =
        (dance dur num denom (b1, b2) (t1, t2)) :+: (dance dur num denom  (b1, b2) (t2, t1))

    magic2 = tempo (92 / 120) magic

    