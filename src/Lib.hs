module Lib () where

import Text.Show.Functions ()

doble :: Double -> Double
doble x = x * 2

type Poder = Personaje -> Personaje

data Personaje = UnPersonaje {
    nombre :: String,
    poderBasico :: Poder,
    superPoder :: Poder,
    superPoderActivo :: Bool,
    vida :: Double
} deriving Show
 
espinas = UnPersonaje "Espinas" bolaEspinosa (granadaDeEspinas 5) True 4800
pamela = UnPersonaje "Pamela" (lluviaDeTuercas "sanadoras" False) torretaCurativa True 9600

bolaEspinosa :: Poder
bolaEspinosa contrincante
  | vida contrincante > 1000 = contrincante {vida = vida contrincante - 1000}
  | otherwise = contrincante {vida = 0}

granadaDeEspinas :: Int -> Poder
granadaDeEspinas radioDeExplosion contrincante
  | radioDeExplosion > 3 && vida contrincante < 800 = contrincante {superPoderActivo = False, vida = 0}
  | radioDeExplosion > 3 = contrincante {nombre = nombre contrincante ++ " Espinas estuvo aqui"}
  | otherwise = bolaEspinosa contrincante

lluviaDeTuercas :: String -> Bool -> Poder
lluviaDeTuercas tipoDeTuerca esEnemigo personaje
  | tipoDeTuerca == "sanadoras" && esEnemigo == False = personaje {vida = vida personaje + 800}
  | tipoDeTuerca == "dañinas" && esEnemigo == True = personaje {vida = vida personaje / 2}
  | otherwise = personaje

torretaCurativa :: Poder
torretaCurativa aliado = aliado {superPoderActivo = True, vida = doble (vida aliado)}