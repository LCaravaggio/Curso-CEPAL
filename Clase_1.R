# Clase 1 - Introduccion a la Ciencia de Datos

# Abramos el proyecto "clase_1.Rproj" y empecemos a ver código!

# mecanismo de asignación

mensaje <- 'Primeras lineas de codigo'
mensaje

# Operaciones simples

x <- 3 * 8
x

# Creamos variables con las que podemos realizar operaciones

cantidad_horas_por_curso <- 3
cantidad_cursos <- 8
horas_de_cursada <- cantidad_horas_por_curso * cantidad_cursos
horas_de_cursada

# Valor lógico

'valor character' == 5

'valor character' != 5

6 > 6

# Valores no disponibles

5 >= NA

NA + 10 * 4

is.na(NA)

# Tipos de objetos en R - vector

vector_numerico <- c(10, 20, 30, 40, 50)

vector_numerico


vector_characteres <- c("a", "e", "i", "o", "u")

vector_characteres

# Tipos de objetos en R - Factor

factor <- factor(c("alto", "medio", "bajo"))

factor

# Tipos de objetos en R - Matriz

mimatriz <- matrix(data=1:36, nrow=6, ncol=6, byrow=FALSE)

mimatriz

# Tipos de objetos en R - Lista

personas <- c("Ana", "Camila", "Juan","Pedro")

edad <- c(30, 36, 21, 54, 18, 42, 61)

altura_aprox <- c(1.7, 1.60, 1.80, 1.63, 1.79, 1.75, 1.59, 1.61)

lista_datos <- list(personas, edad, altura_aprox)

lista_datos

# Tipos de objetos en R - Dataframe: la estrella del día

serie_cemento <- read.csv('cemento-series.csv')

# Funciones para indagar el dataframe

names(serie_cemento)
head(serie_cemento)
summary(serie_cemento)

# Veamos cada resultado a medida que corremos cada línea!

serie_cemento$bolsa
serie_cemento$indice_tiempo
unique(serie_cemento$indice_tiempo)

# Tipo de dato de una variable

typeof(serie_cemento$bolsa)

class(serie_cemento$bolsa)

# Datos ordenados, significa que...

# Cada variable debe tener su propia columna.

# Cada observación debe tener su propia fila. 

# Cada valor debe tener su propia celda.

# Presentamos LA librería que ordena nuestros datos: 

library(tidyverse)

# Suma de los valores de una variable numérica

serie_cemento_granel <- sum(serie_cemento$granel)
serie_cemento_granel # al reescribir el nombre del objeto, R nos lo imprime en la consola

# Ordenamos los valores de una variable numérica, por default de modo ascendente

serie_cemento_granel_ordenada <- arrange(serie_cemento, granel)

(head(serie_cemento_granel_ordenada))

# Ordenamos los valores de una variable numérica de manera descendente

serie_cemento_granel_ordenada <- arrange(serie_cemento, desc(granel))

head(serie_cemento_granel_ordenada)

# Seleccionamos variables

seleccion_cemento_granel <- select(serie_cemento_granel_ordenada, granel)

(head(seleccion_cemento_granel))

# Introducimos el uso del Pipe!

cemento_granel <- serie_cemento %>% 
  arrange(desc(granel)) %>% 
  select(granel)

head(cemento_granel)


# Guardar el resultado del trabajo sobre los datos

write.csv2(cemento_granel,
           file = 'cemento_granel.csv',
           row.names = FALSE)
  
# Ahora...a conocer los archivos .Rmd. 
# Vamos a crear uno propio y conocer sus partes y funcionamiento general
