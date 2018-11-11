module Chicago where
    import Euterpea

    octaves :: [Int] -> Music a -> Music a
    octaves [] mus = rest 0
    octaves (o:os) mus = (transpose (12 * o) mus) :=: (octaves os mus)

    malletify mus = (instrument Vibraphone (octaves [0, 1] mus)) :=: (instrument Marimba (octaves [0, 1] mus))

    intro1 = [a 3 hn, fs 4 hn, c 4 hn, g 4 hn, e 4 hn, b 4 hn, d 4 hn, d 5 hn]
    malletWalkDownLine = [fs 4 wn, c 4 wn, e 4 wn, b 3 wn]
    intro2 = [wnr, wnr, wnr, g 3 qn, a 3 qn, b 3 qn, g 4 qn]
    stringLine = [fs 4 hn, d 4 hn, e 4 hn, c 4 hn, b 3 hn, g 3 hn, g 3 qn, a 3 qn, b 3 qn, g 4 qn]

    guitarVerse = times 16 (line [g 4 sn, a 4 en, snr])
    bassVerseLine = foldl (++) [] (map (\_ -> [d 2 en, d 2 en, a 2 en, d 3 hn, a 2 en]) [1..4])

    pianoVerseLine = (line (
        [fs 4 en, fs 4 en, fs 4 en, fs 4 en, fs 4 en, fs 4 en, fs 4 en, fs 4 en] ++
        [c 4 en, c 4 en, c 4 en, c 4 en, c 4 en, c 4 en, c 4 en, c 4 en] ++
        [c 4 en, c 4 en, c 4 en, c 4 en, c 4 en, c 4 en, c 4 en, c 4 en] ++
        [b 3 en, b 3 en, b 3 en, b 3 en, b 3 en, b 3 en, b 3 en, b 3 en])) :=:

        (line ([wnr, wnr, e 4 qn, e 4 en, e 4 qn, e 4 dqn, wnr]))

    stringVerseLine = octaves [1] (instrument Violin (line
        (
        [fs 4 en, fs 4 en, fs 4 en, fs 4 en, fs 4 en] ++
        [g 4 en, g 4 en, g 4 en, g 4 en, g 4 en, g 4 en, g 4 en, g 4 en] ++
        [a 4 en, a 4 en, a 4 en, a 4 en, a 4 en, a 4 en, a 4 en, a 4 en] ++
        [b 4 en, b 4 en, b 4 en, b 4 en, b 4 en, b 4 en, b 4 en, b 4 en] ++
        [c 5 en, c 5 en, c 5 en])))

    pianoVerse1 = instrument HammondOrgan (pianoVerseLine) :=: instrument RhodesPiano (pianoVerseLine)

    strings = instrument StringEnsemble2 (octaves [0, 2] (line intro2))
    malletsAndStrings = strings :=: (malletify (line intro2))

    violin1Bridge = instrument Violin (line [fs 4 wn, c 4 wn, c 4 wn, b 3 wn])
    violin2Bridge = instrument Violin (line [d 4 wn, e 4 wn, e 4 wn, d 4 wn])
    stringBridge = octaves [1] (violin1Bridge :=: violin2Bridge)

    stringValley :: Music Pitch
    stringValley = instrument StringEnsemble2 (octaves [0, 2] (line stringLine))
    malletValley = malletify (line stringLine)

    electricGuitarChorus = instrument ElectricGuitarClean guitarVerse
    bassVerse = instrument ElectricBassFingered (line bassVerseLine)

    ride = perc RideCymbal1 en
    snare = perc AcousticSnare en
    highFloorTom = perc HighFloorTom en
    lowTom = perc LowTom en
    tamb = perc Tambourine en
    maracas = perc Maracas sn
    crash = perc CrashCymbal1 wn

    tambChorus = times 4 (tamb :+: enr :+: tamb :+: tamb :+: enr :+: tamb :+: tamb :+: enr)
    maracasChorus = times 64 (maracas)
    tambChorus2 = times 16 (tamb :+: enr)
    tambChorus3 = times 4 (tamb :+: dqnr :+: tamb :+: dqnr)
    crashChorus = times 4 crash

    snareChorus = times 4 (dqnr :+: snare :+: qnr :+: snare :+: enr)
    rideChorus = times 4 (ride :+: ride :+: ride :+: ride :+: ride :+: ride :+: ride :+: ride)
    highFloorTomChorus = times 4 (highFloorTom :+: highFloorTom :+: dhnr)
    lowTomChorus = times 4 (qnr :+: lowTom :+: hnr :+: lowTom)

    percBuild = (wnr :+: wnr :+: wnr :+: snare :+: highFloorTom :+: lowTom :+: snare :+: highFloorTom :+: lowTom :+: snare :+: enr)

    percChorus = transpose (0) (snareChorus :=: rideChorus :=: highFloorTomChorus :=: lowTomChorus)

    calmVerseFund = instrument RhodesPiano (transpose 12 (line bassVerseLine))

    malletWalk = malletify (line intro1)
    malletWalkDown = malletify (line malletWalkDownLine)

    intro = malletWalk :=: strings
    chorus = stringValley :=: bassVerse :=: percChorus

    chorusVoxLine = octaves [1] (line ([a 3 qn, qnr, d 4 (hn * (1/3)), d 4 (hn * (1/3)), d 4 (hn * (1/3)), d 4 hn, c 4 hn, hnr, e 4 qn, c 4 qn, b 3 qn, qnr, d 4 qn, b 3 qn]))
    chorusVoxHarmonyLine = octaves [1] (line ([d 4 qn, qnr, fs 4 (hn * (1/3)), fs 4 (hn * (1/3)), fs 4 (hn * (1/3)), fs 4 hn, e 4 hn, hnr, g 4 qn, e 4 qn, d 4 qn, qnr, d 4 qn, b 3 qn]))
    chorusTrumpetLine1 = instrument Trumpet (octaves [0, 1] (line ([hnr, a 4 (hn * (1/3)), a 4 (hn * (1/3)), a 4 (hn * (1/3)), a 4 hn, g 4 hn, dqnr, e 4 sn, snr, b 4 en, g 4 qn, g 4 hn, e 4 sn, snr, b 4 en, g 4 qn, a 4 en])))
    chorusTrumpetLine2 = instrument Trumpet (octaves [0, 1] (line ([a 4 qn, qnr, a 4 (hn * (1/3)), a 4 (hn * (1/3)), a 4 (hn * (1/3)), a 4 hn, g 4 hn, dqnr, e 4 sn, snr, b 4 en, g 4 qn, g 4 hn, enr, g 4 en, fs 4 en, e 4 en, d 4 en])))
    chorusTrumpetLine3 = instrument Trumpet (octaves [0, 1] (line ([d 4 qn, qnr, a 4 (hn * (1/3)), b 4 (hn * (1/3)), c 5 (hn * (1/3)), b 4 hn, g 4 qn, d 4 sn, snr, e 4 hn, e 4 sn, snr, b 4 en, g 4 qn, g 4 hn, e 4 sn, snr, b 4 en, g 4 qn, a 4 en])))
    chorusTrumpetLine4 = instrument Trumpet (octaves [0, 1] (line ([a 4 qn, qnr, a 4 (hn * (1/3)), b 4 (hn * (1/3)), c 5 (hn * (1/3)), b 4 hn, g 4 qn, d 4 sn, snr, e 4 hn, e 4 sn, snr, b 4 en, g 4 qn, g 4 hn, enr, g 4 en, fs 4 en, e 4 en, d 4 en])))
    
    trumpetChorus = (times 4 (chorus :=: tambChorus :=: crashChorus :=: malletValley)) :=: (chorusTrumpetLine1 :+: chorusTrumpetLine2 :+: chorusTrumpetLine3 :+: chorusTrumpetLine4)

    chorusVox = (instrument ChoirAahs chorusVoxLine) :=: (instrument Lead6Voice chorusVoxLine)
    chorusVoxHarmony = (instrument ChoirAahs chorusVoxHarmonyLine) :=: (instrument Lead6Voice chorusVoxHarmonyLine)
    chorusVoxWithHarmony = (instrument ChoirAahs chorusVoxLine) :=: (instrument ChoirAahs chorusVoxHarmony)

    verse2_p1 = (calmVerseFund :=: pianoVerse1 :=: (instrument StringEnsemble2 (octaves [0, 2] (line [fs 4 hn])))) :+: (calmVerseFund :=: pianoVerse1 :=: strings)
    verse2 = verse2_p1 :+: (verse2_p1 :=: (malletWalk :+: (malletWalk :=: intro)))

    outroPiano = octaves [-2, 0] (instrument RhodesPiano (line [fs 4 hn, b 4 hn, e 4 hn, a 4 hn, e 4 hn, g 4 hn, d 4 hn, a 4 hn]))
    outroVocals1 = octaves [-1, 0] (instrument ChoirAahs (line [a 4 dwn, g 4 wn, fs 4 wn, e 4 hn]))
    outroVocals2 = instrument ChoirAahs (line [d 5 dwn, c 5 wn, b 4 wn, a 4 hn]) :=: instrument Lead6Voice ((line [d 5 dwn, c 5 wn, b 4 wn, a 4 hn]) :=: (line [a 4 dwn]))
    outroBass = instrument ElectricBassFingered (line [d 2 (4 * wn)])
    outroPerc = crash :=: (times 20 tamb) :=: (tnr :+: (times 20 tamb)) :=: (snr :+: (times 20 tamb)) :=: (dsnr :+: (times 20 tamb))

    {-transpose (-25) ((tempo (30 / 120) -}
    {- song = transpose (-13) (tempo (125 / 120) ((intro :+: (times 2 chorus) :+: verse2 :+: (times 4 (chorus :=: chorusVox))) :+: (verse2 :=: (times 4 stringVerseLine)) :+: (times 4 (chorus :=: chorusVoxWithHarmony)))) -}
    song = let chSong = (intro :+: (times 2 (chorus :=: electricGuitarChorus)) :+: verse2 :+: (times 4 ((chorus :=: electricGuitarChorus) :=: chorusVox))) :+:
                        (verse2 :=: (times 4 stringVerseLine)) :+:
                        (times 4 ((chorus :=: electricGuitarChorus) :=: chorusVoxWithHarmony) :+: (trumpetChorus)) :+:
                        ((times 2 stringBridge) :+: ((times 2 (malletWalkDown)) :=: (instrument Violin (line [a 4 (bn * 4)])))) :+:
                        (times 2 (pianoVerse1 :=: chorusVox :=: tambChorus2 :=: maracasChorus) :+:
                            (((instrument Violin (line [a 4 (bn * 4)])) :=: (times 2 (pianoVerse1 :=: chorusVox :=: tambChorus2 :=: maracasChorus :=: malletWalk :=: calmVerseFund))) :=: (times 4 (line [wnr]) :+: (percBuild :=: malletsAndStrings)))) :+:
                        (times 4 (percChorus :=: tambChorus3 :=: stringValley :=: malletValley :=: bassVerse :=: chorusVoxWithHarmony :=: stringVerseLine :=: electricGuitarChorus)) :+:
                        trumpetChorus :+:
                        ((times 4 (outroPiano :=: outroVocals1 :=: outroVocals2)) :=: outroBass :=: outroPerc) :+:
                        (times 4 (outroPiano :=: outroVocals1 :=: outroVocals2))
                         
           in tempo (125 / 120) chSong