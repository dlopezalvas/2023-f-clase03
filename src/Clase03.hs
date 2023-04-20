module Clase03 where
import PdePreludat

type Nombre = String
type Altura = Number
type Fuerza = Number

--TUPLAS
type Jovit = (Nombre, Altura)

bilbo :: Jovit
bilbo = ("Bilbo", 125)

rosita :: Jovit
rosita = ("Rosita", 115)

yenny :: Jovit
yenny = ("Yennisifez Lorne", 75)

ary :: Jovit
ary = ("Ariel Airimedez", 103)

type Reputacion = Number

reputacion :: Jovit -> Reputacion
reputacion jovit = length (nombre' jovit) * altura' jovit

altura' :: Jovit -> Altura
altura' jovit = snd jovit

nombre' :: Jovit -> Nombre
nombre' jovit = fst jovit

diferenciaDeAltura :: Jovit -> Jovit -> Altura
diferenciaDeAltura jovit1 jovit2 = abs (altura' jovit1 - altura' jovit2)

--Abriendo la tupla (otra forma de escribirlo)
diferenciaDeAltura2 :: Jovit -> Jovit -> Altura
--diferenciaDeAltura2 (nombre1, altura1) (nombre2, altura2) = abs (altura1 - altura2)
--Como no usamos los nombres, podemos usar la variable anónima _
diferenciaDeAltura2 (_, altura1) (_, altura2) = abs (altura1 - altura2)

type Censo = [Jovit]

unCenso = [ary, yenny, bilbo, rosita]

amanecioConElCenso :: Censo -> Jovit
amanecioConElCenso censo = head censo

seCansoDeEsperar :: Censo -> Jovit
seCansoDeEsperar censo = last censo

type Poblacion = Number

participacion :: Censo -> Poblacion
participacion censo = length censo

elTopTres :: Censo -> [Jovit]
elTopTres censo = take 3 censo

elResto :: Censo -> [Jovit]
elResto censo = drop 3 censo

--Tenemos que agregar Fuerza y si viven en la Cumbancha, ¿cambiamos las tuplas?

{-
type Jovit = (Nombre, Altura, Fuerza, Bool)

bilbo :: Jovit
bilbo = ("Bilbo", 125, 5, True)

rosita :: Jovit
rosita = ("Rosita", 115, 5, True)

yenny :: Jovit
yenny = ("Yennisifez Lorne", 75, 5, True)

ary :: Jovit
ary = ("Ariel Airimedez", 103, 5, True)
-}

--Implica cambiar diferenciaDeAltura, ya no podemos usar fst y snd -> usemos data mejor

--Otra forma de modelar
data Jovit2 = UnJovit {
    nombre :: Nombre,
    altura :: Altura,
    fuerza :: Fuerza,
    esDeLaComarca :: Bool
} deriving Show

bilbo2 :: Jovit2
bilbo2 = UnJovit "Bilbo" 125 5 True

--otra sintaxis
rosita2 :: Jovit2
rosita2 = UnJovit { nombre = "Rosita", altura = 115, fuerza = 5, esDeLaComarca = True }



-- Posible resolución TP1:

--Esto está comentado porque en el archivo existe este tipo Nombre
--type Nombre = String 
type Sucursales = Number
type Empleados = Number

cantidadTotalEmpleados :: Sucursales -> Nombre -> Empleados
cantidadTotalEmpleados sucursales nombreEmpresa = sucursales * cantidadEmpleadosSucursal nombreEmpresa

cantidadEmpleadosSucursal :: Nombre -> Empleados
cantidadEmpleadosSucursal "Acme" = 10
cantidadEmpleadosSucursal nombre 
    | terminaConLetraMenor nombre = letrasIntermedias nombre
    | esCapicua nombre && tieneLetrasPar nombre = letrasIntermedias nombre * 2
    | nombreEsDivisiblePor 3 nombre || nombreEsDivisiblePor 7 nombre = 3
    | otherwise = 0

terminaConLetraMenor :: Nombre -> Bool
terminaConLetraMenor nombre = head nombre > last nombre

esCapicua :: Nombre -> Bool
esCapicua nombre = reverse nombre == nombre

tieneLetrasPar :: Nombre -> Bool
tieneLetrasPar nombre = even (length nombre)

letrasIntermedias :: Nombre -> Number
letrasIntermedias nombre = length nombre - 2

esDivisiblePor :: Number -> Number -> Bool
esDivisiblePor divisor dividendo = mod dividendo divisor == 0

nombreEsDivisiblePor :: Number -> Nombre -> Bool
nombreEsDivisiblePor divisor nombre = esDivisiblePor divisor (length nombre)

