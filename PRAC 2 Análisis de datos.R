title: 'Preprocesado de datos: PEC1. Estadísstica avanzada'
author: "Autor: José Luis Martín Ramos"
date: "Junio 2021"
output:
  html_document:
  highlight: default
number_sections: yes
theme: cosmo
toc: yes
toc_depth: 2
includes:
  in_header: M2.851-PEC-header.html
pdf_document:
  highlight: zenburn
toc: yes
word_document: default
---
  
# Leemos el fichero de datos que vamos a empleare en la PRAC 1
# Para ello importamos la librería que nos va a permitir leer datos desde un fichero
# en formato "csv".
# Asignamos el fichero a un dataset denominado DatosINiciales y los mostramos.

library(readr)
DatosIniciales <- read_csv("C:/ESTRATEGICO/UO Catalunya/2021 Tipología y ciclo de vida de los datos/PRAC 2/all_wc_18_players_fifa.csv")
View(DatosIniciales)

# Para mantener los datos originales, vamos a asignar los datos a un dataset
# que vamos a denominar Datos.

Datos <- DatosIniciales

# Vamos a ver la dimensión del dataset

dim(Datos)

# Obtenemos un resumen del fichero.

summary(Datos)

# Para cada una de las variables del fichero, vamos a mostrar el tipo de variable que es,
# y una primera aproximación a los valores de la misma.

# Para la variable "Name"

str(Datos$team)
table(Datos$team)

# Para comprobar si tenemos registros repetidos, utilizamos la función "unique"
# Comprobamos que los ficheros tienen las mismas variables y las mismas observaciones.
# También podemos mostrar por pantalla los datos únicos.

DatosUnicos = unique(Datos)
print(DatosUnicos)

# Podemos generar un Dataset con los datos duplicados, si los hay.
# e imprimir dicho Dataset.
# Comprobamos que en este caso no hay registros duplicados.

Duplicados = duplicated(Datos)
warnings()
print(Duplicados)

#Vamos a ver como están los datos de la variable "height". Lo hacemos en el fichero inicial.

str(DatosIniciales$height)
table(DatosIniciales$height)

# Realizamos los gráficos de tallos y hojas para las variables numéricas.

boxplot(Datos$number)
boxplot(Datos$height)
boxplot(Datos$weight)
boxplot(Datos$age)
boxplot(Datos$caps)

# Hacemos un análisis de las variables numéricas "height", "weight", "age" y "caps"
# Vemos el tipo de dato y los estadísticos descriptivos básicos y las frecuencias

str(Datos$height)
summary(Datos$height)
table(Datos$height)

str(Datos$weight)
summary(Datos$weight)
table(Datos$weight)

str(Datos$age)
summary(Datos$age)
table(Datos$age)

str(Datos$caps)
summary(Datos$caps)
table(Datos$caps)

# Representamos los datos gráficamente.
# Instalamos las librerías necesarias.
# Cargamos los paquetes R que vamos a usar

library(ggplot2)
library(dplyr)

#Creamos unos gráficos de barras y obtenemos las frecuencias de cada variable

#Variable Altura (height)

ggplot(data = Datos) +
  geom_bar(mapping = aes(x = height), show.legend = TRUE)

#Variable Peso (weight)

ggplot(data = Datos) +
  geom_bar(mapping = aes(x = weight))

#Variable Edad (age)

ggplot(data = Datos) +
  geom_bar(mapping = aes(x = age), show.legend = TRUE)

#Variable Internacionalidades (caps)

ggplot(data = Datos) +
  geom_bar(mapping = aes(x = caps), show.legend = TRUE)

# Redondeamos la variable age y obtenemos una nueva variable Edad

Datos$edad <- round(Datos$age)
Datos$edad

#Variable Edad (age)

ggplot(data = Datos) +
  geom_bar(mapping = aes(x = edad), show.legend = TRUE)

# Vamos a calcular los test de normalidad.
install.packages("moments")
library(moments)

hist(Datos$height)
qqnorm(Datos$height)
qqline(Datos$height)

skewness(Datos$height)
agostino.test(Datos$height)

# Para el peso

hist(Datos$weight)
qqnorm(Datos$weight)
qqline(Datos$weight)

x.test <- shapiro.test(Datos$weight)
print(x.test)


# Para la edad

hist(Datos$edad)
qqnorm(Datos$edad)
qqline(Datos$edad)

x.test <- shapiro.test(Datos$edad)
print(x.test)

# Para las internacionalidades

hist(Datos$caps)
qqnorm(Datos$caps)
qqline(Datos$caps)

x.test <- shapiro.test(Datos$caps)
print(x.test)

# ANÁLISIS DE CORRELACIONES

# Para las variables edad y altura
# Lo primero es hacer una representación de los datos

plot(Datos$edad~Datos$weight)

# Calculamos el valor del coeficiente de correlación

cor.test(Datos$edad, Datos$weight)

# Para las variables peso y altura
# Lo primero es hacer una representación de los datos

plot(Datos$height~Datos$weight)

# Calculamos el valor del coeficiente de correlación

cor.test(Datos$height, Datos$weight)

# Vamos a proceder a la realización del modelo de regresión lineal simple.

# Hay que decir que la estimación de un valor perdido comentado en un apartado anterior,
# se puede hacer de distintas formas. Una de ellas sería la regresión lineal.

# PARA EL MODELO DE ALTURA EN FUNCIÓN DEL PESO

# Lo primero es hacer una representación de los datos

plot(Datos$height~Datos$weight)

# Calculamos el valor del coeficiente de correlación

cor.test(Datos$height, Datos$weight)

# obtenemos el modelo lineal de la altura en función del peso.

modelo_altura <- lm (Datos$height~Datos$weight)

# Mostramos los parámetros del modelo.

summary(modelo_altura)

# Ahora lo coeficientes.

modelo_altura$coefficients

# Por último, mostramos el gráfico con la línea de regresión.

plot(Datos$height~Datos$weight)
abline(modelo_altura)

# TEST DE HIPÓTESIS

# Procedemos a la selección de los grupos a comparar.

Spain = filter(Datos, Datos$team == "Spain")
Spain
Egypt = filter(Datos, Datos$team == "Egypt")
Egypt

# Vamos a realizar un test de medias T-test para cada una de las variables de interés.

# Importamos las librerías necesarias.

library(openintro)
library(tidyverse)

# Creamos los grupos de España y Egipto seleccionando los registros que
# cumplen la condición de que el team sea Spain o Egypt.

España <- Datos %>% filter(team == "Spain") %>% pull(edad)
Egipto <- Datos %>% filter(team == "Egypt") %>% pull(edad)

# Calculamos las medias y las desviaciones típicas.

mean(España)
mean(Egipto)
sd(España)
sd(Egipto)

# Otro ejemplo

Argentina <- Datos %>% filter(team == "Argentina") %>% pull(edad)
Francia <- Datos %>% filter(team == "France") %>% pull(edad)

# Calculamos las medias y las desviaciones típicas.

mean(Argentina)
mean(Francia)
sd(Argentina)
sd(Francia)

# Podemos ver la diferencia de medias de forma visual.

mean(Argentina) - mean(Francia)

# Aplicamos un test de medias para los dos grupos creados.

wilcox.test(x = Argentina, y = Francia, alternative = "two.sided", mu = 0,
            paired = FALSE, conf.int = 0.95)

# Representación de datos.

# Instalamos la librería necesaria.

install.packages("RcmdrMisc")

require(RcmdrMisc)

numSummary(Datos[,"edad", drop=FALSE],
           statistics=c("mean", "sd", "se(mean)", "IQR", "quantiles", "cv", "skewness",
                        "kurtosis"), quantiles=c(0,.25,.5,.75,1), type="2")
binnedCounts(Datos[,"edad", drop=FALSE])

hist(Datos$edad, col = "Red", main = "Histograma de edades",
     ylab = "Frecuecia", xlab = "Edades")

boxplot(Datos$edad, col = "deepskyblue",  main = "Edades")
points(mean(Datos$edad),pch=10)

head(Datos, 10)
