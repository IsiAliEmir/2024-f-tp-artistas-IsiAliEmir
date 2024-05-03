module Library where
import PdePreludat
import Data.Char (isLower)

-- Tenemos información sobre distintos Artistas y sus Canciones (se pueden agregar más):

type Cancion = String

data Artista = UnArtista {
    nombre :: String,
    canciones :: [Cancion]
} deriving Show

fitito :: Artista
fitito = UnArtista "Fitito Paez" ["11 y 6", "El amor despues del amor", "Mariposa Tecknicolor"]

calamardo :: Artista
calamardo = UnArtista "Andres Calamardo" ["Flaca", "Sin Documentos", "Tuyo siempre"]

paty :: Artista
paty = UnArtista "Taylor Paty" ["Shake It Off", "Lover"]

palmeiras :: Artista
palmeiras = UnArtista "Los Palmeiras" ["Saboreando cumbias", "El bombon", "Volo la paloma"]

ricardito :: Artista
ricardito = UnArtista "Ricardito Arshona" ["Quesos, cosas, casas", "Señora de las cuatro decadas", "Marta"]

-- 1) Determinar la calificacion de una canción, que equivale a la cantidad de letras minúsculas
-- (sin espacios, números ni caracteres especiales) de la canción, más 10.

esMinuscula :: Char -> Bool
esMinuscula unaLetra = unaLetra >= 'a' && unaLetra <= 'z'

cantMinusculas :: String -> Number
cantMinusculas [] = 0
cantMinusculas (primeraLetra:restoDeLaPalabra)
    | esMinuscula primeraLetra = 1 + cantMinusculas restoDeLaPalabra
    | otherwise = cantMinusculas restoDeLaPalabra

calificacion :: Cancion -> Number
calificacion unaCancion = cantMinusculas unaCancion + 10

-- 2) Averiguar si es exitoso un artista, lo que sucede cuando la suma de las calificaciones buenas 
-- de sus canciones es mayor a 50 (son buenas las que tienen calificacion mayor a 20).

esBuena :: Cancion -> Bool
esBuena unaCancion = calificacion unaCancion > 20

buenasCanciones :: Artista -> [Cancion]
buenasCanciones unArtista = filter esBuena (canciones unArtista)

buenasCalificaciones :: Artista -> [Number]
buenasCalificaciones unArtista = map calificacion (buenasCanciones unArtista)

sumaBuenasCalificaciones :: Artista -> Number
sumaBuenasCalificaciones unArtista = sum (buenasCalificaciones unArtista)

esExitoso :: Artista -> Bool
esExitoso unArtista = sumaBuenasCalificaciones unArtista > 50

-- 3) Obtener todos los artistas exitosos, a partir de un conjunto de artistas.

artistasExitosos :: [Artista] -> [Artista]
artistasExitosos = filter esExitoso -- es lo mismo que: artistasExitosos conjuntoDeArtistas = filter esExitoso conjuntoDeArtistas

-- 4) Hacer todo lo anterior en una función definida en una sola línea, sin definir funciones auxiliares.