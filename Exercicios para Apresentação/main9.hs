--9.1
main :: IO ()
main = (readLn :: IO Int) >>= \ x -> putStrLn $ show (x * 2)

main' :: IO Int
main' = (readLn :: IO Int) >>= \ x -> return (x * 2)


--9.2
main92' :: IO ()
main92' = getLine >>= \ valorDigitado -> 
         putStrLn $ reverse valorDigitado
         
main92 :: IO ()
main92 = putStrLn "Programa reverte Paravra" >>
         putStrLn "Digite uma palavra:" >>
         getLine >>= \ valorDigitado -> 
         putStrLn $ reverse valorDigitado
         
--9.3 
data Jogo = Pedra | Papel | Tesoura deriving (Show,Enum,Read,Eq)

partida :: Jogo -> Jogo -> Jogo
partida Pedra Papel = Papel
partida Pedra Tesoura = Pedra
partida Pedra Pedra = Pedra
partida Tesoura Papel = Tesoura
partida Tesoura Pedra = Pedra
partida Tesoura Tesoura = Tesoura
partida Papel Tesoura = Tesoura
partida Papel Pedra = Papel
partida Papel Papel = Papel

main94 :: IO ()
main94 = do
    jogadasPossiveis <- return $ take 10 $ cycle [Pedra .. Tesoura]
    let jogadaComputador = jogadasPossiveis !! 2
    putStrLn $ "Jogada do computador foi: " ++ show jogadaComputador
    putStrLn "Digite uma jogada: Pedra ou Papel ou Tesoura" 
    jogada <- (readLn :: IO Jogo)
    
    putStrLn $ if jogada == jogadaComputador then "empate"
               else
                acertou $ partida jogada jogadaComputador == jogada
    where
        acertou True = "Venceu"
        acertou False = "Perdeu"
        
  --  putStrLn $ acertou $ partida jogadaComputador jogada == jogada 
 
    