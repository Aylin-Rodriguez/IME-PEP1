library(tidyr)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(pwr)
library(magrittr)
library(tidyverse)
library(RVAideMemoire)
library(rcompanion)

setwd("C:/Users/aylin/OneDrive/Desktop/IME-PEP1")
datos <- read.csv2("PEP 1 Datos.csv")
#PRUEBA PRÁCTICA FORMA 4
# Fijar una semilla para asegurar que los resultados sean reproducibles.
set.seed(478)

nmuestra <- 400
cat("\nPregunta 1\n")

# Esta pregunta corresponde a una inferencia acerca de la diferencia entre
# dos proporciones. Se utilizará  el método de Wilson, pero ahora
# para la diferencia entre dos proporciones.

# Comencemos por formular las hipótesis.
# H0: La proporción de bajas es la misma en Salsacia y
#     Conservia (p1 - p2 = 0).
# HA: La proporción de bajas es distinta en Salsacia y
#     Conservia (p1 - p2 != 0).


#Se definen  valores conocidos.
abolladosSalsacia <- datos[datos[["Pais"]] == "Salsacia",][datos[["Estado"]] =="Abollado",]
abiertosSalsacia <- datos[datos[["Pais"]] == "Salsacia",][datos[["Estado"]] =="Abierto",]
sanosSalsacia <- datos[datos[["Pais"]] == "Salsacia",][datos[["Estado"]] =="Sano",]
abolladosConservia <- datos[datos[["Pais"]] == "Conservia",][datos[["Estado"]] =="Abollado",]
abiertosConservia <- datos[datos[["Pais"]] == "Conservia",][datos[["Estado"]] =="Abierto",]
sanosConservia <- datos[datos[["Pais"]] == "Conservia",][datos[["Estado"]] =="Sano",]
nAbolladosSals <- length(abolladosSalsacia)
nAbiertosSals <- length(abiertosSalsacia)
nSanosSals <- length(sanosSalsacia)
nAbolladosCon <- length(abolladosConservia)
nAbiertosCon <- length(abiertosConservia)
nSanosCon <- length(sanosConservia)
bajasSalsacia <- nAbolladosSals + nAbiertosSals
bajasConservia <- nAbolladosCon + nAbiertosCon
nSalsacia <- bajasSalsacia + nSanosSals
nConservia <- bajasConservia + nSanosCon


# Ahora se verifican las condiciones.
# se puede suponer que los autores del estudio inicial fueron rigurosos y que se
# cumple la condición de independencia.

# La condición de éxito fracaso establece que se espera observar al menos 10
# observaciones correspondientes a éxito (bajas, en este caso) y al menos 10,
# correspondientes a fracasos (sanos) en cada una de las muestras, lo que
# podemos verificar fácilmente en los datos.

# Una vez verificadas las condiciones, procedemos con la prueba. No tenemos
# motivos para ser más exigentes, por lo que fijaremos un nivel de significación
# (alfa) de 0,05.

prueba <- prop.test(x = c(bajasSalsacia, bajasConservia),
                      n = c(nSalsacia, nConservia),
                      alternative = "two.sided", conf.level = 0.90,
                      correct = FALSE)

print(prueba)

# El valor p obtenido, 1, es mucho mayor que el nivel de significación de
# 0,05, por lo que fallamos al rechazar la hipótesis nula. Concluimos entonces,
# con 95% de confianza, que la proporción de bajas en Salsacia y COnsercia
# es la misma.

cat("\nPregunta 2\n")
intConf <- 0.95
pExitoSalsacia <- bajasSalsacia / nSalsacia
pExitoConservia <- bajasConservia / nConservia
tamEfecto <- ES.h(pExitoSalsacia, pExitoConservia)

poder_estadistico <- pwr.2p.test(h = tamEfecto,
                                 n = NULL,
                                 sig.level = 1 - intConf,
                                 alternative = "two.sided")

cat("\nPregunta 3\n")

# Diferencia de proporciones entre dos
# grupos, pero ahora se solicita el tamaño de la muestra.

# se tamaño de las muestras, que tienen tamaños iguales.
intConf <- 0.95
poder <- 0.80
# poder_estadistico2 <- pwr.2p.test(h = tamEfecto,
#                                  n = NULL,
#                                  sig.level = 1-intConf,
#                                  power = poder,
#                                  alternative = "two.sided")

cant2 <- bsamsize(p1 = pExitoSalsacia,
                 p2 = pExitoConservia,
                 fraction = nSalsacia / (nSalsacia + nConservia),
                 alpha = 1- intConf,
                 power = 0.8)
print(cant2)

