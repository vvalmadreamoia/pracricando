-- Parcial Minecraft

-- Craft
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

-- Mine
{-El mundo del videojuego se compone de biomas, donde cada bioma tiene muchos materiales.
Para poder minar en un bioma particular el personaje debe tener un elemento necesario según el bioma.
Por ejemplo, en un bioma ártico, donde hay hielo, iglues y lobos, se debe tener un suéter.
Cuando un personaje va a minar a un bioma, si cuenta con el elemento necesario, agrega a su inventario uno de los materiales del bioma y gana 50 puntos.
La forma de elegir cuál es el material del bioma a conseguir, depende de la herramienta que use al minar.
Por ejemplo, el hacha hace que se mine el último de los materiales del bioma, mientras que la espada actúa sobre el primero de ellos.
Existe tambien el pico, que por ser más preciso permite apuntar a una determinada posición de los materiales.
Por ejemplo, si un personaje con un sueter en su inventario mina el artico con un pico de precisión 1, agrega un iglú a su inventario.
En caso de no poder minar por no tener lo necesario el personaje se va con las manos vacías y sigue como antes.

Hacer una función minar, que dada una herramienta, un personaje y un bioma, permita obtener cómo queda el personaje.
Definir las herramientas mencionadas y agregar dos nuevas. Mostrar ejemplos de uso. Hacerlo de manera que agregar en el futuro otras herramientas no implique modificar la función minar.
Utilizando la función composición, usar una que permita obtener un material del medio del conjunto de materiales.
Utilizando una expresión lambda, inventar una nueva herramienta, diferente a las anteriores

¿Qué pasa al intentar minar en un bioma con infinitos materiales? Mostrar ejemplos donde con diferentes herramientas o personajes sucedan diferentes cosas. Justificar.
-}

data Bioma = UnBioma{
    elemNB :: Material,
    elemBioma :: [Material]
} deriving (Show, Eq)
artico, desierto :: Bioma

artico = UnBioma{
    elemNB = sueter,
    elemBioma = [hielo]
}

desierto = UnBioma{
    elemNB = fogata,
    elemBioma = [madera]
}

hielo :: Material
hielo = UnMaterial{ 
    nombrem = "Hielo",
    tiempo = 10,
    receta = []
}

data Herramienta = UnaHerramienta{
    precision :: Int
} deriving (Show, Eq)

hacha, espada, pico :: Herramienta

hacha = UnaHerramienta{
    precision = 1
}
espada = UnaHerramienta{
    precision = 1
}
pico = UnaHerramienta{
    precision = 2 --a fines de simplificar
}

minar :: Personaje -> Herramienta -> Bioma -> Personaje
minar p h b
    |tienemateriales (elemNB b) p = p{inventario = inventario p ++ [materialobtenido h b], puntaje = puntaje p + 50}
    |otherwise = p

materialobtenido :: Herramienta -> Bioma-> Material
materialobtenido h b
    |h == hacha = head (elemBioma b)
    |h == espada = last (elemBioma b)
    |h == pico = (elemBioma b) !! precision h
    |otherwise = error "herramienta no disponible"

materialdelmedio :: [Material]-> Material
materialdelmedio listam =  listam !! (div(length listam) 2)