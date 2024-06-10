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
sacarHasta10 energia= max 0. sacar10 $ energia

sacar10 :: Int->Int
sacar10 energia = subtract 10 energia

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

ramon :: Perro
ramon = UnPerro "Dalmata" ["Peluche","Pelota","Mantita","Peine"] 20000 20

guarderiaPdePerritos :: Guarderia
guarderiaPdePerritos = UnaGuarderia "GuarderíaPdePerritos" [(jugar,30),(ladrar 18,20),(regalar "Pelota",0),(diaDeSpa,120),(diaDeCampo,720)]

puedeEstarEnUnaGuarderia :: Perro->Guarderia->Bool
puedeEstarEnUnaGuarderia (UnPerro _ _ tiempoAPermanecer _) (UnaGuarderia _ rutina) = (tiempoAPermanecer >=).tiempoTotalRutina $ rutina

tiempoTotalRutina :: [Actividad]->Int
tiempoTotalRutina unaRutina = sum. tiemposRutina $ unaRutina

tiemposRutina :: [Actividad]->[Int]
tiemposRutina unaRutina = map snd unaRutina

responsables :: [Perro]->[Perro]
responsables unosPerros = filter esResponsable unosPerros

esResponsable :: Perro->Bool
esResponsable unPerro = tieneMasDeTresJuguetes.diaDeCampo $ unPerro

tieneMasDeTresJuguetes :: Perro->Bool
tieneMasDeTresJuguetes unPerro = (>=3).length.juguetesFavoritos $ unPerro

realizarRutina :: Perro->Guarderia->Perro
realizarRutina unPerro unaGuarderia
    | puedeEstarEnUnaGuarderia unPerro unaGuarderia = hacerRutina unPerro.rutina $ unaGuarderia
    | otherwise = unPerro

hacerRutina :: Perro->[Actividad]->Perro
hacerRutina unPerro unaRutina = foldl (\perro ejercicio -> ejercicio perro) unPerro. ejerciciosRutina $ unaRutina

ejerciciosRutina :: [Actividad]->[Ejercicio]
ejerciciosRutina unaRutina = map  fst unaRutina 

quedanCansados :: [Perro]->Guarderia->[Perro]
quedanCansados unosPerros unaGuarderia= filter (quedaCansadoPostRutina unaGuarderia) unosPerros

quedaCansadoPostRutina :: Guarderia->Perro->Bool
quedaCansadoPostRutina unaGuarderia unPerro = tieneEnergiaMenorA5.flip realizarRutina unaGuarderia $ unPerro

tieneEnergiaMenorA5 :: Perro->Bool
tieneEnergiaMenorA5 (UnPerro _ _ _ energia)= energia <5

piPerro :: Perro
piPerro = UnPerro "Labrador" infinitasSoguitas 314 159
--uso piPerro en lugar de pi, ya que sino la consola intenta usar la constante numérica pi

infinitasSoguitas :: [Juguete]
infinitasSoguitas = map soguita [1..]

soguita :: Int->Juguete
soguita unNumero= "Soguita " ++ show unNumero

{-
1) Es posible saber si piPerro es de raza extravagante, ya que, como Haskell usa Lazy Evaluation, 
primero evalúa la función para saber qué necesita del parámetro y luego la aplica. Así, la función solo
debe considerar la raza del perro, por lo que no necesita analizar los juguetes, los cuales son una lista infinita.

ghci> esDeRazaExtravagante piPerro
False

2)a. Al intentar saber si dentro de la lista de juguetes de piPerro, la cual es una lista infinita, se encuentra
"Huesito", el programa se quedará evaluando infinitamente uno por uno los elementos de la lista hasta encontrarlo.
Si "Huesito" sí formara parte de la lista, entonces daría True.

elem "Huesito". juguetesFavoritos $ piPerro
Interrupted.

b. Ocurriría lo mismo que en el item b.

ghci> elem "Pelota".juguetesFavoritos.flip realizarRutina guarderiaPdePerritos $ piPerro
Interrupted.

c. El programa evalúa la lista infinita hasta encontrar "Soguita 31112". Como la encuentra, no se 
queda procesando el resto de la lista, sino que devuelve True.

elem "Soguita 31112". juguetesFavoritos $ piPerro      
True

3) Es posible que piPerro realice una rutina, ya que hacerlo no implica tener que evaluar la lista infinita de juguetes.
Regalar es el único ejercicio que utiliza la lista de juguetes favoritos de piPerro y ni siquiera debe evaluarla, sino que agrega un 
juguete al inicio de dicha lista.
El programa debe devolver a piPerro

ghci> realizarRutina piPerro guarderiaPdePerritos
UnPerro {raza = "Labrador", juguetesFavoritos = ["Soguita 1","Soguita 2","Soguita 3","Soguita 4", Interrupted.

4)La funcion regalar agrega un juguete al comienzo de la lista de juguetes. Por eso, 
puede agregar un juguete en los juguetes favoritos de piPerro. Si el juguete se agregara al final,
entonces el programa quedaría procesando la lista infinita indefinidamente ya que no llegará al final.

ghci> regalar "Hueso" piPerro
UnPerro {raza = "Labrador", juguetesFavoritos = ["Hueso","Soguita 1","Soguita 2","Soguita 3",Interrupted.

-}