-- 9.4

baskara :: Float -> Float -> Float -> (Float, Float)
baskara a b c =
        let delta = sqrt (b^2 - 4*a*c)
        in ( (-b-delta)/(2*a),
             (-b+delta)/(2*a) )
raiz = do
        putStrLn "Digite os coeficientes"
        putStrLn "a:"
        a <- getLine
        putStrLn "b:"
        b <- getLine
        putStrLn "c:"
        c <- getLine
        let raiz = baskara (read a) (read b) (read c)
        putStrLn ("Raizes:" ++ show (fst(raiz)) ++ " - " ++ show (snd(raiz)))