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
 
espinas :: Personaje
espinas = UnPersonaje "Espinas" bolaEspinosa (granadaDeEspinas 5) True 4800
pamela :: Personaje
pamela = UnPersonaje "Pamela" (lluviaDeTuercas "sanadora") torretaCurativa False 9600

bolaEspinosa :: Poder
bolaEspinosa contrincante
  | vida contrincante > 1000 = contrincante {vida = vida contrincante - 1000}
  | otherwise = contrincante {vida = 0}

granadaDeEspinas :: Int -> Poder
granadaDeEspinas radioDeExplosion contrincante
  | radioDeExplosion > 3 && vida contrincante < 800 = contrincante {superPoderActivo = False, vida = 0}
  | radioDeExplosion > 3 = contrincante {nombre = nombre contrincante ++ " Espinas estuvo aqui"}
  | otherwise = bolaEspinosa contrincante

lluviaDeTuercas :: String -> Poder
lluviaDeTuercas tipoDeTuerca personaje
  | tipoDeTuerca == "sanadora" = personaje {vida = vida personaje + 800}
  | tipoDeTuerca == "dañina" = personaje {vida = vida personaje / 2}
  | otherwise = personaje

torretaCurativa :: Poder
torretaCurativa aliado = aliado {superPoderActivo = True, vida = doble (vida aliado)}

atacarConPoderEspecial :: Personaje -> Poder
atacarConPoderEspecial personaje contrincante
  | superPoderActivo personaje = superPoder personaje . poderBasico personaje $ contrincante 
  | otherwise = contrincante

estaEnLasUltimas :: Personaje -> Bool
estaEnLasUltimas personaje = vida personaje < 800