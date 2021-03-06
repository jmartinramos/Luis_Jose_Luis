title: 'Preprocesado de datos: PEC1. Estad�sstica avanzada'
author: "Autor: Jos� Luis Mart�n Ramos"
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
# Para ello importamos la librer�a que nos va a permitir leer datos desde un fichero
# en formato "csv".
# Asignamos el fichero a un dataset denominado DatosINiciales y los mostramos.

library(readr)
DatosIniciales <- read_csv("C:/ESTRATEGICO/UO Catalunya/2021 Tipolog�a y ciclo de vida de los datos/PRAC 2/all_wc_18_players_fifa.csv")
View(DatosIniciales)

# Para mantener los datos originales, vamos a asignar los datos a un dataset
# que vamos a denominar Datos.

Datos <- DatosIniciales

# Vamos a ver la dimensi�n del dataset

dim(Datos)

# Obtenemos un resumen del fichero.

summary(Datos)

# Para cada una de las variables del fichero, vamos a mostrar el tipo de variable que es,
# y una primera aproximaci�n a los valores de la misma.

# Para la variable "Name"

str(Datos$team)
table(Datos$team)

# Para comprobar si tenemos registros repetidos, utilizamos la funci�n "unique"
# Comprobamos que los ficheros tienen las mismas variables y las mismas observaciones.
# Tambi�n podemos mostrar por pantalla los datos �nicos.

DatosUnicos = unique(Datos)
print(DatosUnicos)

# Podemos generar un Dataset con los datos duplicados, si los hay.
# e imprimir dicho Dataset.
# Comprobamos que en este caso no hay registros duplicados.

Duplicados = duplicated(Datos)
warnings()
print(Duplicados)

#Vamos a ver como est�n los datos de la variable "height". Lo hacemos en el fichero inicial.

str(DatosIniciales$height)
table(DatosIniciales$height)

# Realizamos los gr�ficos de tallos y hojas para las variables num�ricas.

boxplot(Datos$number)
boxplot(Datos$height)
boxplot(Datos$weight)
boxplot(Datos$age)
boxplot(Datos$caps)

# Hacemos un an�lisis de las variables num�ricas "height", "weight", "age" y "caps"
# Vemos el tipo de dato y los estad�sticos descriptivos b�sicos y las frecuencias

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

# Representamos los datos gr�ficamente.
# Instalamos las librer�as necesarias.
# Cargamos los paquetes R que vamos a usar

library(ggplot2)
library(dplyr)

#Creamos unos gr�ficos de barras y obtenemos las frecuencias de cada variable

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

# AN�LISIS DE CORRELACIONES

# Para las variables edad y altura
# Lo primero es hacer una representaci�n de los datos

plot(Datos$edad~Datos$weight)

# Calculamos el valor del coeficiente de correlaci�n

cor.test(Datos$edad, Datos$weight)

# Para las variables peso y altura
# Lo primero es hacer una representaci�n de los datos

plot(Datos$height~Datos$weight)

# Calculamos el valor del coeficiente de correlaci�n

cor.test(Datos$height, Datos$weight)

# Vamos a proceder a la realizaci�n del modelo de regresi�n lineal simple.

# Hay que decir que la estimaci�n de un valor perdido comentado en un apartado anterior,
# se puede hacer de distintas formas. Una de ellas ser�a la regresi�n lineal.

# PARA EL MODELO DE ALTURA EN FUNCI�N DEL PESO

# Lo primero es hacer una representaci�n de los datos

plot(Datos$height~Datos$weight)

# Calculamos el valor del coeficiente de correlaci�n

cor.test(Datos$height, Datos$weight)

# obtenemos el modelo lineal de la altura en funci�n del peso.

modelo_altura <- lm (Datos$height~Datos$weight)

# Mostramos los par�metros del modelo.

summary(modelo_altura)

# Ahora lo coeficientes.

modelo_altura$coefficients

# Por �ltimo, mostramos el gr�fico con la l�nea de regresi�n.

plot(Datos$height~Datos$weight)
abline(modelo_altura)

# TEST DE HIP�TESIS

# Procedemos a la selecci�n de los grupos a comparar.

Spain = filter(Datos, Datos$team == "Spain")
Spain
Egypt = filter(Datos, Datos$team == "Egypt")
Egypt

# Vamos a realizar un test de medias T-test para cada una de las variables de inter�s.

# Importamos las librer�as necesarias.

library(openintro)
library(tidyverse)

# Creamos los grupos de Espa�a y Egipto seleccionando los registros que
# cumplen la condici�n de que el team sea Spain o Egypt.

Espa�a <- Datos %>% filter(team == "Spain") %>% pull(edad)
Egipto <- Datos %>% filter(team == "Egypt") %>% pull(edad)

# Calculamos las medias y las desviaciones t�picas.

mean(Espa�a)
mean(Egipto)
sd(Espa�a)
sd(Egipto)

# Otro ejemplo

Argentina <- Datos %>% filter(team == "Argentina") %>% pull(edad)
Francia <- Datos %>% filter(team == "France") %>% pull(edad)

# Calculamos las medias y las desviaciones t�picas.

mean(Argentina)
mean(Francia)
sd(Argentina)
sd(Francia)

# Podemos ver la diferencia de medias de forma visual.

mean(Argentina) - mean(Francia)

# Aplicamos un test de medias para los dos grupos creados.

wilcox.test(x = Argentina, y = Francia, alternative = "two.sided", mu = 0,
            paired = FALSE, conf.int = 0.95)

# Representaci�n de datos.

# Instalamos la librer�a necesaria.

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
