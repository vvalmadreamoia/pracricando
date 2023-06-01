-- Parcial Minecraft
data Personaje = UnPersonaje {
    nombre:: String,
    puntaje:: Int,
    inventario:: [Material]
} deriving (Show, Eq)

juan :: Personaje
juan = UnPersonaje {
    nombre = "Juan",
    puntaje = 20,
    inventario = [fogata]
}

data Material = UnMaterial{
    nombrem :: String,
    tiempo :: Int,
    receta :: [Material]
} deriving (Show, Eq)

fogata, polloAsado, sueter, lana, madera :: Material
fogata = UnMaterial{
    nombrem = "Fogata",
    tiempo = 10,
    receta = [madera]
}
polloAsado = UnMaterial{
    nombrem = "Pollo asado",
    tiempo = 300,
    receta = [fogata]
}
sueter = UnMaterial{
    nombrem = "Sueter",
    tiempo = 600,
    receta = [lana]
}
lana = UnMaterial{
    nombrem = "Lana",
    tiempo = 10,
    receta =[]
}
madera = UnMaterial{
    nombrem = "Madera",
    tiempo = 15,
    receta =[]
}

type Receta = [Material]

tienemateriales :: Material -> Personaje -> Bool
tienemateriales m p = all (\x -> elem x (inventario p)) (receta m)

craftear :: Personaje -> Material -> Personaje
craftear p m
    |tienemateriales m p = p{inventario = filter (\x -> not (elem x (receta m))) (inventario p) ++ [m], puntaje = puntaje p + 10 * (tiempo m)} 
    |otherwise = p

duplica :: Personaje -> Material -> Bool
duplica p m = puntaje (craftear p m) >= (puntaje p)*2

objetosQueDuplican :: Personaje -> [Receta] -> [Material]
objetosQueDuplican p rs = filter (duplica p) (concat rs)

craftearSucesivamente :: Personaje -> [Receta] -> Personaje
craftearSucesivamente p rs = foldl craftear p (concat rs)

craftearSucREVERSE :: Personaje -> [Receta] -> Personaje
craftearSucREVERSE p rs = foldr (flip craftear) p (concat rs)

puntajeMayor :: Personaje -> [Receta] -> Personaje
puntajeMayor p rs
    |puntaje (craftearSucesivamente p rs) > puntaje (craftearSucREVERSE p rs) = craftearSucesivamente p rs
    |puntaje (craftearSucREVERSE p rs) > puntaje (craftearSucesivamente p rs) = craftearSucREVERSE p rs
    |otherwise = error "igual puntaje"