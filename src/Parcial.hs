module Parcial where
import Text.Show.Functions()

doble :: Int -> Int
doble = (*2)

data Perro = UnPerro{
    raza :: String,
    juguetesFavoritos :: [Juguete],
    tiempo :: Minutos,
    energia :: Int
} deriving Show

type Juguete = String
type Minutos = Int

data Guarderia = UnaGuarderia{
    nombreGuarderia :: String,
    rutina :: [Actividad]
} deriving Show

type Actividad = (Ejercicio, Tiempo)
type Tiempo = Int
type Ejercicio = Perro->Perro

modificarEnergia :: (Int->Int)->Perro->Perro
modificarEnergia unaFuncion unPerro = unPerro {energia = unaFuncion.energia $ unPerro}

jugar :: Ejercicio
jugar unPerro = modificarEnergia sacarHasta10 unPerro

sacarHasta10 :: Int->Int
sacarHasta10 energia= subtract (min 10 energia) energia

ladrar :: Int->Ejercicio
ladrar ladridos unPerro = modificarEnergia (aumentarMitad ladridos) unPerro

aumentarMitad :: Int->Int->Int
aumentarMitad ladridos energia = energia + (div ladridos 2)

regalar:: Juguete->Ejercicio
regalar unJuguete unPerro = agregarJuguete unJuguete unPerro

agregarJuguete :: Juguete->Perro->Perro
agregarJuguete unJuguete unPerro = modificarJuguetesFavoritos (unJuguete:) unPerro

modificarJuguetesFavoritos :: ([Juguete]->[Juguete])->Perro->Perro
modificarJuguetesFavoritos unaFuncion unPerro = unPerro{juguetesFavoritos = unaFuncion.juguetesFavoritos $ unPerro}

diaDeSpa :: Ejercicio
diaDeSpa unPerro
    | esDeRazaExtravaganteOPermaneceAlMenos50Minutos unPerro = recompensaSpa unPerro
    | otherwise = unPerro

recompensaSpa :: Perro->Perro
recompensaSpa unPerro = agregarJuguete "Peine de goma".modificarEnergia (const 100) $ unPerro

esDeRazaExtravaganteOPermaneceAlMenos50Minutos :: Perro->Bool
esDeRazaExtravaganteOPermaneceAlMenos50Minutos unPerro = esDeRazaExtravagante unPerro || permaneceAlMenos50Minutos unPerro

esDeRazaExtravagante:: Perro->Bool
esDeRazaExtravagante unPerro = flip elem ["Dalmata","Pomerania"].raza $ unPerro

permaneceAlMenos50Minutos :: Perro->Bool
permaneceAlMenos50Minutos unPerro = (>= 50).tiempo $ unPerro

diaDeCampo :: Ejercicio
diaDeCampo unPerro = perderPrimerJuguete.jugar $ unPerro

perderPrimerJuguete :: Perro->Perro
perderPrimerJuguete unPerro = modificarJuguetesFavoritos tail unPerro

zara :: Perro
zara = UnPerro "Dalmata" ["Pelota","Mantita"] 90 80

guarderiaPdePerritos :: Guarderia
guarderiaPdePerritos = UnaGuarderia "GuarderíPdePerritos" [(jugar,30),(ladrar 18,20),(regalar "Pelota",0),(diaDeSpa,120),(diaDeCampo,720)]
