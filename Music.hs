module SimpleMusic where
    import Euterpea
   
    -- mkScale :: Pitch -> [Int] -> Music Pitch
    -- mkScale p ints = map (trans () p) ints

    log2 :: [Int] -> [Int -> Int]
    log2 ints = map (+) ints

    leeg :: [Int -> Int] -> Int -> [Int]
    leeg [] init = []
    leeg (fn:fns) init = ((fn init):(fn (leeg fns init)))

