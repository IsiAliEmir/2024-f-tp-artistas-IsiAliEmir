module Library where
import PdePreludat

-- Tenemos información sobre distintos Artistas y sus Canciones (se pueden agregar más):

type Cancion = String

data Artista = UnArtista {
    nombre :: String,
    canciones :: [Cancion]
} deriving Show

fitito :: Artista
fitito = UnArtista "Fitito Paez" ["11 y 6", "El amor despues del amor", "Mariposa Tecknicolor"]

calamardo :: Artista
calamardo = UnArtista "Andres Calamardo" ["Flaca", "Sin Documentos", "Tuyo Siempre"]

paty :: Artista
paty = UnArtista "Taylor Paty" ["Shake It Off", "Lover"]

marian :: Artista
marian = UnArtista "Marian MG" ["La Cumbia del Otaku", "Los Juegos del Calamar", "Metal del Gatito"]

ricardito :: Artista
ricardito = UnArtista "Ricardito Arshona" ["Quesos, cosas, casas", "Señora de las cuatro decadas"]

misArtistas = [fitito, calamardo, paty, marian, ricardito]

-- 1) Determinar la calificacion de una canción, que equivale a la cantidad de letras minúsculas
-- (sin espacios, números ni caracteres especiales) de la canción, más 10.

minusculas :: String
minusculas = "abcdefghijklmnñopqrstuvwxyz"

esMinuscula :: Char -> Bool
esMinuscula = (`elem` minusculas)

calificacion :: Cancion -> Number
calificacion = (+10) . length . filter esMinuscula

-- 2) Averiguar si es exitoso un artista, lo que sucede cuando la suma de las calificaciones buenas 
-- de sus canciones es mayor a 50 (son buenas las que tienen calificacion mayor a 20).

esBuena :: Cancion -> Bool
esBuena = (>20) . calificacion

buenasCanciones :: Artista -> [Cancion]
buenasCanciones = filter esBuena . canciones

buenasCalificaciones :: Artista -> [Number]
buenasCalificaciones = map calificacion . buenasCanciones

esExitoso :: Artista -> Bool
esExitoso = (>50) . sum . buenasCalificaciones

-- 3) Obtener todos los artistas exitosos, a partir de un conjunto de artistas.

artistasExitosos :: [Artista] -> [Artista]
artistasExitosos = filter esExitoso

-- 4) Hacer todo lo anterior en una función definida en una sola línea, sin definir funciones auxiliares.

artistasExitosos' :: [Artista] -> [Artista]
artistasExitosos' = filter ((>50) . sum . (map ((+10) . length . filter (`elem` "abcdefghijklmnñopqrstuvwxyz")) . (filter ((>20) . (+10) . length . filter (`elem` "abcdefghijklmnñopqrstuvwxyz")) . canciones)))
