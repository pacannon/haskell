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

    phraseDur = (4 * wn)

    partA1 = instrument StringEnsemble1 (octaves [1] (part [(C, 4), (G, 4), (E, 3)] [en, dqn, hn] phraseDur))
    partB1 = instrument BrassSection (octaves [0] (part [(E, 4), (G, 4), (C, 3)] [qn, hn, qn] phraseDur))
    partC1 = instrument Lead8BassLead (octaves [-1] (part [(E, 4)] [dqn + qn] phraseDur))
    partD1 = instrument Bassoon (octaves [-2] (part [(C, 4)] [dqn, wn, dqn + qn] phraseDur))

    partA2 = instrument StringEnsemble1 (octaves [1] (part [(A, 4), (E, 4), (C, 3)] [hn] phraseDur))
    partB2 = instrument BrassSection (octaves [0] (part [(A, 4), (E, 4), (C, 3)] [qn, hn, qn] phraseDur))
    partC2 = instrument Lead8BassLead (octaves [-1] (part [(A, 4)] [dqn + qn] phraseDur))
    partD2 = instrument Bassoon (octaves [-2, -1] (part [(E, 4)] [dqn, wn, dqn + qn] phraseDur))

    partA3 = instrument StringEnsemble1 (octaves [1] (part [(F, 4), (A, 4), (C, 3)] [en, en, en] phraseDur))
    partB3 = instrument BrassSection (octaves [0] (part [(C, 4), (C, 5), (C, 3), (A, 5), (C, 4), (C, 5), (C, 3)] [dqn, sn, sn, en, en, sn, en, sn, en, qn, sn, sn, en, en, sn, en, sn, en, qn] phraseDur))
    partC3 = instrument Lead8BassLead (octaves [-1] (part [(A, 4), (C, 4), (F, 3)] [hn + en] phraseDur))
    partD3 = instrument Bassoon (octaves [-2] (part [(A, 4)] [en, qn, en, qn, qn] phraseDur))

    partA4 = instrument StringEnsemble1 (octaves [1] (part [(F, 4), (A, 3), (G, 4)] [qn, en, dqn] phraseDur))
    partB4 = instrument BrassSection (octaves [0] (part [(A, 4), (D, 4), (F, 3)] [dhn, dhn, hn] phraseDur))
    partC4 = instrument Lead8BassLead (octaves [-1] (part [(F, 4)] [qn, en] phraseDur))
    partD4 = instrument Bassoon (octaves [-2] (part [(D, 4)] [(wn / 5)] phraseDur))

    partA5 = instrument StringEnsemble1 (octaves [1] (part [(C, 4), (E, 4), (G, 3)] [hn] phraseDur))
    partB5 = instrument BrassSection (octaves [0] (part [(G, 4)] [2 * (qn / 3), 2 * (qn / 3), qn / 3, 2 * (qn / 3), 2 * (qn / 3), 2 * (qn / 3)] phraseDur))
    partC5 = instrument Lead8BassLead (octaves [-1] (part [(E, 4), (C, 4), (G, 3)] [(dqn + qn) / 4] phraseDur))
    partD5 = instrument Bassoon (octaves [-2] (part [(C, 4)] [dqn, wn, dqn + qn] phraseDur))

    partA6 = instrument StringEnsemble1 (octaves [1] (part [(E, 4), (G, 4), (B, 3)] [(wn / 5)] phraseDur))
    partB6 = instrument BrassSection (octaves [0] (part [(G, 4), (E, 4), (B, 3)] [qn, hn, qn] phraseDur))
    partC6 = instrument Lead8BassLead (octaves [-1] (part [(B, 4), (B, 5)] [dqn + qn] phraseDur))
    partD6 = instrument Bassoon (octaves [-2] (part [(E, 4)] [dqn, wn, dqn + qn] phraseDur))

    song =
        times 1 (times 2 (
        partA1 :=: partB1 :=: partC1 :=: partD1 :+:
        partA1 :=: partB2 :=: partC1 :=: partD2 :+:
        partA2 :=: partB2 :=: partC2 :=: partD2 :+:
        partA2 :=: partB3 :=: partC2 :=: partD3 :+:
        partA3 :=: partB3 :=: partC3 :=: partD3 :+:
        partA3 :=: partB1 :=: partC3 :=: partD1) :+:
        times 1 (
        partA4 :=: partB4 :=: partC4 :=: partD4 :+:
        partA5 :=: partB4 :=: partC5 :=: partD4 :+:
        partA5 :=: partB5 :=: partC5 :=: partD5 :+:
        partA6 :=: partB5 :=: partC6 :=: partD5 :+:
        partA6 :=: partB6 :=: partC6 :=: partD6 :+:
        partA4 :=: partB6 :=: partC4 :=: partD6))
