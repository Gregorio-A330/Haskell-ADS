module EXG2 where

--data Metro = Metro

--type Yard = 0,914

--data Inch = "0.0254" deriving Show

--data Foot = 0.3048


-- Sabe-se	que	as	unidades imperiais	de	comprimento	podem ser Inch,
-- Yard ou Foot (há outras ignoradas aqui). Sabe-se que 1in=0.0254m, 1yd=0.9144m, 1ft=0.3048.
-- Faça	a função converterMetros que recebe a unidade imperial e o valor correspondente	nesta unidade.
-- Esta função deve retornar o valor em metros. Implemente também a função converterImperial, que recebe
-- um valor	em metros e a unidade de conversão. Esta função deve retornar o valor convertido para a
-- unidade desejada.

converterMetros :: String -> String
converterMetros inch = "0.0254m"
converterMetros yard = "0.9144m"
converterMetros foot = "0.3048m"

converterIn :: Double -> Double
converterIn x = x * 0.0254

converterYd :: Double -> Double
converterYd x = x * 0.9144