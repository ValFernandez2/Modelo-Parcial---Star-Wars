--Alumno: Valentín Fernández Pizzella

module Library where
import PdePreludat

--PUNTO 1
--Modelado de naves del enunciado y una nueva

data Nave = UnaNave {
    nombre :: String,
    durabilidad :: Number,
    escudo :: Number,
    ataque :: Number,
    poderes :: Poder
} deriving (Show, Eq)

--Naves
tieFighter = UnaNave "TIE FIghter" 200 100 50 turbo

xWing = UnaNave "X Wing" 300 150 100 reparacionDeEmergencia

tieAvanzado = UnaNave "Nave de Darth Vader" 500 300 200 superTurbo

milleniumFalcon = UnaNave "Millenium Falcon" 1000 500 50 (reparacionDeEmergencia.modificarEscudos 100)

--Modelo una nueva nave
razorCrest = UnaNave "Razor Crest" 250 250 100 turboDeEmergencia

--Poderes
type Poder = Nave -> Nave

--Funciones auxiliares
modificarDurabilidad :: Number -> Poder
modificarDurabilidad numero nave = nave {durabilidad = restaNoNegativa (durabilidad nave) numero}

modificarAtaque :: Number -> Poder
modificarAtaque numero nave = nave {ataque = restaNoNegativa (ataque nave) numero}

modificarEscudos :: Number -> Poder
modificarEscudos numero nave = nave {escudo = escudo nave + numero}

restaNoNegativa :: Number -> Number -> Number
restaNoNegativa n m = max 0 (n - m)

--Poderes
turbo :: Poder
turbo = modificarAtaque 25

reparacionDeEmergencia :: Poder
reparacionDeEmergencia = modificarDurabilidad 50.modificarAtaque (-30)

superTurbo :: Poder
superTurbo = modificarDurabilidad 45.turbo.turbo.turbo

--Invento un nuevo poder
turboDeEmergencia :: Poder
turboDeEmergencia = turbo.reparacionDeEmergencia


--PUNTO 2
--Durabilidad de una flota
type Flota = [Nave]

durabilidadTotal :: Flota -> Number
durabilidadTotal flota = sum (map durabilidad flota)


--PUNTO 3
--Ataque de otra nave
atacar :: Nave -> Nave -> Nave
atacar nave1 nave2 = intentoDeAtaque (activarPoder nave1) (activarPoder nave2)

intentoDeAtaque :: Nave -> Nave -> Nave
intentoDeAtaque nave1 nave2 | ataque nave1 > escudo nave2 = dañar nave1 nave2
                            | otherwise = modificarDurabilidad 0 nave2

dañar :: Nave -> Nave -> Nave
dañar nave1 nave2 = modificarDurabilidad (restaNoNegativa (ataque nave1) (escudo nave2)) nave2

activarPoder :: Nave -> Nave
activarPoder nave = (poderes nave) nave


--PUNTO 4
--Nave Fuera de Combate
fueraDeCombate :: Nave -> Bool
fueraDeCombate nave = durabilidad nave == 0


--PUNTO 5
--Flota enemiga luego de una estrategia
type Estrategia = Nave -> Bool

navesDebiles :: Estrategia
navesDebiles nave = escudo nave < 200

navesPeligrosas :: Number -> Estrategia
navesPeligrosas numero nave = ataque nave > numero

navesFueraDeCombate :: Nave -> Estrategia
navesFueraDeCombate nave = fueraDeCombate . atacar nave

--Invento una nueva estrategia
navesConPocaDurabilidad :: Estrategia
navesConPocaDurabilidad nave = durabilidad nave <= 200

ataqueSorpresa :: Estrategia -> Nave -> Flota -> Flota
ataqueSorpresa estrategia nave = mapSelectivo (atacar nave) estrategia

mapSelectivo :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapSelectivo cambio condicion lista = map cambio (filter condicion lista) ++ filter (not.condicion) lista


--PUNTO 6
--Determinar que estrategia es mejor
mision :: Estrategia -> Estrategia -> Nave -> Flota -> Flota
mision estrategia1 estrategia2 nave flota = ataqueSorpresa (mejorEstrategia estrategia1 estrategia2 nave flota) nave flota

mejorEstrategia :: Estrategia -> Estrategia -> Nave -> Flota -> Estrategia
mejorEstrategia estrategia1 estrategia2 nave flota | durabilidadTotal (ataqueSorpresa estrategia1 nave flota) < durabilidadTotal (ataqueSorpresa estrategia2 nave flota) = estrategia1
                                                   | otherwise = estrategia2


--PUNTO 7
--Flota infinita de naves
flotaInfinita :: [Flota]
flotaInfinita flota = cycle flota

{- Para determinar la durabilidad total de la flota, debemos obtener la durabilidad de cada una
de las naves, lo cual no es posible ya que no podemos recorrer por completo una lista infinita.
 Para llevar a cabo una misión (la elección de una estrategia sobre otra) debemos evaluar cual es más
conveniente según la reducción de la durabilidad total de la flota, y como indicado en la respuesta
anterior, esto no es posible, por lo que tampoco es posible llevar a cabo una misión sobre una flota
infinita

-}