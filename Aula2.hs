module Aula2 where

data Void

data Dia = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado deriving (Eq, Show, Ord, Enum)

data Day = Sunday | Monday| Thurday | Wednesday | Tuesday | Friday | Saturday deriving Show

data Mes = Jan | Fev | Mar | Abr | Mai | Jun | Jul | Ago | Set | Out | Nov | Dez deriving Show

data Calendario = Calendario Int Dia Mes deriving Show 

proxMes :: Calendario -> Calendario
proxMes (Calendario num dia Jan) = Calendario num dia Fev
proxMes (Calendario num dia Fev) = Calendario num dia Mar



-- Pattern Matching: DESCONTROI OS TIPOS, OU SEJA, EXPÕE SEU VALOR DE MODO A CRIAR MANEIRAS 
-- DIFERENTES DE SE MANIPULAR UMA FUNÇÃO, USA-SE O PATTERN MATCHING COMO UM CASE.

agenda :: Dia -> String
agenda Segunda = "Dia de Tristeza"
agenda Terca   = "CHUVA"
agenda Quarta  = "Dia de Haskell"
agenda Sexta   = "Dia de Maldade"
agenda _       = "Dia que ninguem liga... "

-- 1) CRIE UM A FUNCAO toDia que transforma um inteiro, recebido via parametro em Dia.
-- 1 para Domingo, 2 para Segunda... 7 para Sabado.

-- toDia EH UMA FUNCAO PARCIAL
-- NAO HA COBERTURA PARA TODOS OS PATTENRS
toDia :: Int -> Either String Dia

toDia 1 = Right Domingo
toDia 2 = Right Segunda
toDia 3 = Right Terca
toDia 4 = Right Quarta
toDia 5 = Right Quinta
toDia 6 = Right Sexta
toDia 7 = Right Sabado
toDia _ = Left "ERRO"


-- 2) Crie uma funcao toNum que converte Dia em Inteiro de acordo com a tabela acima.
-- toNum: eh uma funcao total
-- todas as entradas estao cobertas
toNum :: Dia -> Int

toNum Domingo = 1
toNum Segunda = 2
toNum Terca = 3
toNum Quarta = 4
toNum Quinta = 5
toNum Sexta = 6
toNum Sabado = 7

-- 3) Faça o tipo Day contendo os valores Sunday, ..., Saturday e crie uma função
-- chamada traduzir que recebe um Dia e retorna um Day de acordo com a tradução para o Portugues.

translate :: Dia -> Day

translate Domingo = Sunday
translate Segunda = Monday
translate Terca = Thurday
translate Quarta = Wednesday
translate Quinta = Tuesday
translate Sexta = Friday
translate Sabado = Saturday

