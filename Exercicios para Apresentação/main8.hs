-- 8.1
data Caixa a = Um a | Dois a a | Tres a a a deriving Show

-- Exemplo polimorfico com a assumindo ser tipo Integer

caixa1 = Um 1
caixa2 = Dois 2 4
caixa3 = Tres 2 4 6

instance Functor Caixa where
    fmap funcao (Um x1)         = Um (funcao x1)
    fmap funcao (Dois x1 x2 )   = Dois (funcao x1) (funcao x2)
    fmap funcao (Tres x1 x2 x3) = Tres (funcao x1) (funcao x2) (funcao x3)
    

-- fmap (+1) caixa1
-- fmap (+1) caixa2
-- fmap (+1) caixa3
-- fmap (+1) (Um 2)

--A partir desta versão do GHCI, toda Monada deve ter instância de Applicative Functor
--Ignore esta parte
instance Applicative Caixa where

instance Monad Caixa where
    return x1                     = Um x1
    Um x1 >>= funcao              = funcao x1
    Dois (_) (x2) >>= funcao      = funcao x2
    Tres (_) (_) (x3) >>= funcao  = funcao x3










-------------------------------------------------------------    
-- 8.2

mult234 :: Double -> Caixa Double
mult234 x1 = Um x1 >>= \ valor -> Tres (x1*2) (x1*3) (x1*4)
--      x1 = fmap (*2) (Um x1)