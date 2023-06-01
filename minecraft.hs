-- Parcial Minecraft
data Personaje = UnPersonaje {
    nombre:: String,
    puntaje:: Int,
    inventario:: [Material]
} deriving Show