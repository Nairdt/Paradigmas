module MicroEntrega1 where
data Microprocesador = Microprocesador {memoria :: [Int], acumuladorA :: Int, acumuladorB :: Int, programCounter :: Int, mensajeError :: String} deriving Show

xt8088 = Microprocesador{memoria=(replicate 1024 0), acumuladorA=0,acumuladorB=0,programCounter=0,mensajeError=" "}

nop :: Microprocesador -> Microprocesador
nop = sumarPC

add :: Microprocesador -> Microprocesador
add micro = sumarPC micro {acumuladorA = acumuladorA micro + acumuladorB micro, acumuladorB = 0}

divide :: Microprocesador -> Microprocesador
divide micro 
            |acumuladorB micro == 0 = sumarPC micro {mensajeError = "DIVISION BY ZERO"}
            |otherwise = sumarPC micro {acumuladorA = div (acumuladorA micro) (acumuladorB micro), acumuladorB = 0}

swap :: Microprocesador -> Microprocesador
swap micro = sumarPC micro {acumuladorA = acumuladorB micro, acumuladorB = acumuladorA micro}

lod :: Int -> Microprocesador -> Microprocesador
lod addr micro = sumarPC micro {acumuladorA = (!!) (memoria micro) (addr - 1)}

str :: Int -> Int -> Microprocesador -> Microprocesador 
str addr val micro = sumarPC micro {memoria = (take (addr - 1) (memoria micro)) ++ [val] ++ (drop addr (memoria micro)) }

lodv :: Int -> Microprocesador -> Microprocesador
lodv val micro = sumarPC micro {acumuladorA = val}

sumarPC :: Microprocesador -> Microprocesador
sumarPC micro = micro {programCounter = programCounter micro + 1}

-- 3.3.2 -> (show.add.(lodv 22).swap.(lodv 10)) xt8088

-- 3.4.2 -> (show.divide.(lod 1).swap.(lod 2).(str 2 0).(str 1 2)) xt8088

-- 4.1 -> (show.nop.nop.nop) xt8088

-- 4.3 -> (show.divide.(lodv 12).swap.(lodv 4)) xt8088

fp20 = Microprocesador{memoria=[], acumuladorA=7,acumuladorB=24,programCounter=0,mensajeError=" "}

at8086 = Microprocesador{memoria=[1..20], acumuladorA=0,acumuladorB=0,programCounter=0,mensajeError=" "}
