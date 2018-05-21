module MicroEntrega2 where

import Text.Show.Functions

data Microprocesador = Microprocesador {memoria :: [Int], instruccion :: [(Microprocesador -> Microprocesador)], acumuladorA :: Int, acumuladorB :: Int, programCounter :: Int, mensajeError :: String} deriving Show

xt8088 = Microprocesador{memoria=(replicate 1024 0), instruccion = [], acumuladorA=0,acumuladorB=0,programCounter=0,mensajeError=" "}

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

fp20 = Microprocesador{memoria=[], instruccion = [], acumuladorA=7,acumuladorB=24,programCounter=0,mensajeError=" "}

at8086 = Microprocesador{memoria=[1..20], instruccion = [], acumuladorA=0,acumuladorB=0,programCounter=0,mensajeError=" "}

laSumaDe10Y22 = [(lodv 10), swap, (lodv 22), add]

laDivisionDe2Por0 = [(str 1 2), (str 2 0), (lod 2), swap, (lod 1), divide]

cargarPrograma :: Microprocesador -> [(Microprocesador -> Microprocesador)] -> Microprocesador
cargarPrograma micro listaProg = micro {instruccion = listaProg}

ejecutarPrograma :: Microprocesador -> Microprocesador
ejecutarPrograma (Microprocesador mem [] acumA acumB pc " ") = (Microprocesador mem [] acumA acumB pc "NO PROGRAM DETECTED")
ejecutarPrograma (Microprocesador mem (x:[]) acumA acumB pc " ") = x (Microprocesador mem [] acumA acumB pc " ")
ejecutarPrograma (Microprocesador mem (x:xs) acumA acumB pc " ") = ejecutarPrograma (x (Microprocesador mem xs acumA acumB pc " "))
ejecutarPrograma micro = micro

-- Pruebas de que funciona: 
--	-> ejecutarPrograma(cargarPrograma xt8088 laDivisionDe2Por0) 				-> el acumulador a es 2, el b es 0, el progCounter es 6
--	-> ejecutarPrograma(cargarPrograma xt8088 (divide:laDivisionDe2Por0))		-> el acumulador a es 0, el b es 0, el progCounter es 1

ifnz :: [(Microprocesador -> Microprocesador)] -> Microprocesador -> Microprocesador
ifnz listaInstrucciones micro
                             |(acumuladorA micro) == 0 = micro
                             |otherwise = (salvarLista (ejecutarPrograma (micro {instruccion = listaInstrucciones})) (instruccion micro))

salvarLista :: Microprocesador -> [(Microprocesador -> Microprocesador)] -> Microprocesador
salvarLista micro listaInst = micro {instruccion = listaInst}

-- Prueba de que funciona: ifnz [(lodv 3), swap] (fp20 {instruccion = [swap]})   -> acumulador A = 24, el B = 3 y aun conserva su programa "swap"

depurar :: [(Microprocesador -> Microprocesador)] -> [(Microprocesador -> Microprocesador)]
depurar = filter (quedaEn0 xt8088)

quedaEn0 :: Microprocesador -> (Microprocesador -> Microprocesador) -> Bool
quedaEn0 micro instruc = not ((sumaValores (instruc micro) == 0) && memoriaVacia (instruc micro))

sumaValores :: Microprocesador -> Int
sumaValores (Microprocesador _ _ acumA acumB _ _) = (acumA + acumB)

memoriaVacia :: Microprocesador -> Bool
memoriaVacia micro = all (== 0) (memoria micro)

-- Prueba de que funciona: depurar [swap, nop, (lodv 133), (lodv 0), (str 1 3), (str 2 0)]   -> me devuelve solo 2 functions

microDesorden = Microprocesador {memoria = [2, 5, 1, 0, 6, 9], instruccion = [], acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = " "}

tieneMemoriaOrdenada :: Microprocesador -> Bool
tieneMemoriaOrdenada (Microprocesador (x:[]) _ _ _ _ _) = True
tieneMemoriaOrdenada (Microprocesador (x1:x2:xs) inst acumA acumB pc error) = (x1 <= x2) && tieneMemoriaOrdenada (Microprocesador (x2:xs) inst acumA acumB pc error)

microInfinito = Microprocesador{memoria=(repeat 0), instruccion = [], acumuladorA=0,acumuladorB=0,programCounter=0,mensajeError=" "}

-- Memoria Infinita -> el compilador no tiene problema al cargar el codigo con una lista infinita, pero al momento de ejecutar alguna funcion sobre el,
--					   se queda atrapado en un loop infinito mostrando por pantalla el valor que se repite indefinidamente, pero al ejecutar la funcion si
--					   la memoria esta ordenada, se queda en el mismo loop pero esta ves sin mostrar por pantalla.
--					   Se puede suponer que en el primer caso el ciclo infinito esta al queder mostrar el contenido de la memoria por pantalla, mientras
--					   que en el segundo el bucle ocurre en medio de la ejecucion de la funcion, en este caso en la parte recursiva, no logrando llegar
--					   a conseguir el resultado que debe dar (un Bool)