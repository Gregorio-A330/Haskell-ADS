--3.17)	Faça	o	tipo		Cripto		que	possua	dois	values	constructors
--	Mensagem		 e	 	Cifrado	,	 ambos	 com	 um	 campo	 	String		 e	 um
--value	 constructor	 	 Erro	 .	 Faça	 as	 funções	 	 encriptar	 	 e
--	decriptar	,	seguindo	cada	exemplo	a	seguir.
--Prelude>	encriptar	(Mensagem		"FATEC")	
--Cifrado	"GBUFD"	
--Prelude>	decriptar	(Cifrado		"DBTB")	
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
	