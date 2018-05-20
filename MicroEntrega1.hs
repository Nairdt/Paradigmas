module MicroEntrega1 where
data Microprocesador = Microprocesador {mem :: [Int], acumA :: Int, acumB :: Int, pC :: Int, mensaje :: String} deriving Show

xt8088 = Microprocesador{mem=[], acumA=0,acumB=0,pC=0,mensaje=" "}

nop :: Microprocesador -> Microprocesador
nop = sumarPC

add :: Microprocesador -> Microprocesador
add micro = sumarPC micro {acumA = acumA micro + acumB micro, acumB = 0}

divide :: Microprocesador -> Microprocesador
divide micro 
            |acumB micro == 0 = sumarPC micro {mensaje = "DIVISION BY ZERO"}
            |otherwise = sumarPC micro {acumA = div (acumA micro) (acumB micro), acumB = 0}

swap :: Microprocesador -> Microprocesador
swap micro = sumarPC micro {acumA = acumB micro, acumB = acumA micro} 

lod :: Int -> Microprocesador -> Microprocesador
lod addr micro = sumarPC micro {acumA = (!!) (mem micro) (addr - 1)}

str :: Int -> Int -> Microprocesador -> Microprocesador 
str addr val micro = sumarPC micro {mem = (take (addr - 1) (mem micro)) ++ [val] ++ (drop addr (mem micro)) }

lodv :: Int -> Microprocesador -> Microprocesador
lodv val micro = sumarPC micro {acumA = val}

sumarPC :: Microprocesador -> Microprocesador
sumarPC micro = micro {pC = pC micro +1}


-- 3.3.2 -> (show.add.(lodv 22).swap.(lodv 10)) xt8088

-- 3.4.2 -> (show.divide.(lod 1).swap.(lod 2).(str 2 0).(str 1 2)) xt8088

-- 4.1 -> (show.nop.nop.nop) xt8088

-- 4.3 -> (show.divide.(lodv 12).swap.(lodv 4)) xt8088


f20 = Microprocesador{mem=[], acumA=7,acumB=24,pC=0,mensaje=" "}

at8086 = Microprocesador{mem=[1..20], acumA=0,acumB=0,pC=0,mensaje=" "}

programCounter :: Microprocesador -> Int
programCounter micro = pC micro


acumuladorA :: Microprocesador -> Int
acumuladorA micro = acumA micro

acumuladorB :: Microprocesador -> Int
acumuladorB micro = acumB micro

memoria :: Microprocesador -> [Int]
memoria micro = mem micro

mensajeError :: Microprocesador -> String
mensajeError micro = mensaje micro