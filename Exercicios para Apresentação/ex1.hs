
--3.5)	Sabe-se	que	as	unidades imperiais	de	comprimento	podem
--ser Inch, Yard ou	Foot (há outras	ignoradas aqui). Sabe-se
--que 1in=0.0254m, 1yd=0.9144m, 1ft=0.3048. Faça a função
--converterMetros que recebe a unidade imperial e o valor
--correspondente nesta unidade. Estafunção deve retornar o valor
--em metros.
--Implemente também a função converterImperial, que
--recebe um valor em metros	e a	unidade	de conversão. Esta função
--deve retornar	o valor	convertido para	a unidade desejada



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
