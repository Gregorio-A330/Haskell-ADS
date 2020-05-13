-- Gabriel Gregorio do Couto RA: 50481423013
-- Peter Menezes             RA: 50481723026

--3.5)	Sabe-se	que	as unidades imperiais de	comprimento	podem
--ser Inch, Yard ou	Foot (há outras	ignoradas aqui). Sabe-se
--que 1in=0.0254m, 1yd=0.9144m, 1ft=0.3048. Faça a função
--converterMetros que recebe a unidade imperial e o	valor
--correspondente nesta unidade.	Esta função deve retornar o valor
--em metros.
--Implemente também a função converterImperial, que
--recebe um	valor em metros	e a	unidade	de conversão. Esta função
--deve retornar	o valor	convertido para	a unidade desejada.


module Quinto where
    data Unidades = Inch | Yard | Foot

    converterMetros :: Unidades -> Double -> Double
    converterMetros Inch x = x * 0.0254
    converterMetros Yard x = x * 0.9144
    converterMetros Foot x = x * 0.3048
    converterMetros _ x = x

    converterImperial :: Unidades -> Double -> Double
    converterImperial Inch x = x / 0.0254
    converterImperial Yard x = x / 0.9144
    converterImperial Foot x = x / 0.3048
    converterImperial _ x = x


--3.17)	Faça o tipo	 Cripto	 que possua	dois values	constructors
--	Mensagem e Cifrado, ambos com um campo String e um
--value constructor	Erro. Faça as funções encriptar e
--decriptar, seguindo cada exemplo a seguir.
--Prelude>	encriptar (Mensagem	 "FATEC")	
--Cifrado	"GBUFD"	
--Prelude>	decriptar (Cifrado "DBTB")	
--Mensagem	"CASA"


module DecimoSetimo where
    data Cripto = Mensagem String | Cifrado String | Erro deriving Show

    encriptar :: Cripto -> Cripto
    encriptar (Mensagem x) = Cifrado [ succ y | y <- x]
    encriptar (Cifrado z) = Erro
    encriptar _ = Erro

    decriptar :: Cripto -> Cripto
    decriptar (Cifrado x) = Mensagem [ pred y | y <- x]
    decriptar (Mensagem z) = Erro
    decriptar _ = Erro	
	
--3.21)	Crie a função maxMoeda que recebe uma lista de moedas
--e	retorna	o valor	máximo absoluto	(sem conversão alguma) dentre
--os campos val	desta lista.	Exemplo:
--Prelude> maxMoeda	[Moeda 3 Real, Moeda 7 Dollar, Moeda 1 Euro]	
--7
-- Use a função maximum	.
	
module VigesimoPrimeiro where
    data Cambio = Euro | Real | Dollar deriving Show
    data Moeda = Moeda {val :: Double, cur :: Cambio} deriving Show

    converteMoedas :: Moeda -> Cambio -> Moeda
    converteMoedas (Moeda x Real) Euro = Moeda (x * 0.22) Euro
    converteMoedas (Moeda x Dollar) Euro = Moeda (x * 0.91) Euro
    converteMoedas (Moeda x Dollar) Real = Moeda (x * 4.16) Real
    converteMoedas (Moeda x Euro) Real = Moeda (x * 4.58) Real
    converteMoedas (Moeda x Real) Dollar = Moeda (x * 0.24) Dollar
    converteMoedas (Moeda x Euro) Dollar = Moeda (x * 1.1) Dollar
    converteMoedas (Moeda x y) z = Moeda x z

    converterTodosReal :: [Moeda] -> [Moeda]
    converterTodosReal mdList = [ converteMoedas md Real | md <- mdList ]
    
    maxMoeda :: [Moeda] -> Double
    maxMoeda mdList = maximum [ val md | md <- mdList ]