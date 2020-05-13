import Control.Monad
import System.Directory

data Pergunta = Sim | Nao

main1 :: IO ()
main1 = 
    putStrLn "Digite um numero: " >>
    readLn >>= \ x ->
    putStrLn "Digite outro numero: " >>
    readLn >>= \ y ->
    putStrLn $ "Resultado e: " ++ (show $ x + y)


main11 :: IO () -> Pergunta -> Int
main11 = 
    putStrLn "Digite algo " >>
    Pergunta <- getLine 
        pergNum Sim >= 1
        pergNum Nao >= 0

--------------------------------------------------------------------------------------------------------------------------------------------
main2 :: IO ()
main2 = putStrLn "Qual seu nome?" >>
        getLine >>= \ nome ->
            if (nome == "") then
                putStrLn "Erro... " >> main2
            else
                (putStrLn $ "Ola " ++ nome) >>
                putStrLn "Fim"

--------------------------------------------------------------------------------------------------------------------------------------------
main3 :: IO ()
main3 = readLn >>= \ z ->
    forM_ [1..z] $ (\i -> putStrLn $ show i)

{-

main4 :: IO	() 
main4 =	do 
    z <- readLn
        let	dentro i = do
            putStrLn $ "Número " ++ (show i)
        readLn
        ns <- mapM dentro	[1..z]
        putStrLn $ "Resultado: " ++ (show $ sum ns)

--------------------------------------------------------------------------------------------------------------------------------------------
data Naipe = Ouros | Espadas | Copas | Paus	deriving (Show, Enum)

data Valor = Dois | Tres | Quatro | Cinco | Seis |
             Sete | Oito | Nove	| Dez | J | Q | K | A
             deriving (Show, Enum)

data Carta	= Carta {valor :: Valor, naipe :: Naipe} deriving Show

main5 :: IO () 
main = do
    let	acertou	True = "Você acertou"
        acertou	False = "Errou..."
        baralho	<- return [Carta x y | x<-[Dois	.. A], y<-[Ouros ..	Paus]]
        cartaNum <- randomRIO (1, length baralho)
        carta <- return $ baralho !! cartaNum
        putStrLn "Escreva a	carta para adivinhar: "
        palpite	<- readLn
        putStrLn $ "Sua carta foi " ++ show (valor carta) ++ " de "	+ +	show (naipe	carta)
        putStrLn $ acertou $ carta == palpite

--------------------------------------------------------------------------------------------------------------------------------------------
import	Text.Printf
main :: IO ()
main = do
    lista <- fmap (map words . lines) $ readFile "func.dat"
    salarios <- return $ map (\(_:vl:_) -> read vl) lista :: IO [Double]
    printf "%.2f\n" $ sum salarios
    print $ maximum salarios


main7 :: IO ()
main7 = do
    putStrLn "Digite o nome do arquivo. Será criado	caso não exista"
    arq	<- getLine
    putStrLn "Digite uma mensagem"
    mensagem <- getLine existe <- doesFileExist	arq
    if existe then
        appendFile arq ("\n" ++ mensagem)
    else
        writeFile arq mensagem
-}