--Alumno: Valentín Fernández Pizzella

module Library where
import PdePreludat

--PUNTO 1
--Modelado de naves del enunciado y una nueva
type Poder = Nave -> Nave

data Nave = UnaNave {
    nombre :: String,
    durabilidad :: Number,
    escudo :: Number,
    ataque :: Number,
    poderes :: [Poder] 
} deriving (Show, Eq)

--Naves
tieFighter = UnaNave "TIE FIghter" 200 100 50 [turbo]

xWing = UnaNave "X Wing" 300 150 100 [reparacionDeEmergencia]

tieAvanzado = UnaNave "Nave de Darth Vader" 500 300 200 [superTurbo]

milleniumFalcon = UnaNave "Millenium Falcon" 1000 500 50 [reparacionDeEmergencia, modificarEscudos 100]

otraNave = UnaNave 

--Poderes
turbo :: Poder
turbo nave = modificarAtaque 25 nave

reparacionDeEmergencia :: Poder
reparacionDeEmergencia = modificarDurabilidad 50.modificarAtaque 30

superTurbo :: Poder
superTurbo = modificarDurabilidad 45.turbo.turbo.turbo

modificarDurabilidad :: Number -> Poder
modificarDurabilidad numero nave = nave {durabilidad = durabilidad nave + numero}

modificarAtaque :: Number -> Poder
modificarAtaque numero nave = nave {ataque = ataque nave + numero}

modificarEscudos :: Number -> Poder
modificarEscudos numero nave = nave {escudo = escudo nave + numero}