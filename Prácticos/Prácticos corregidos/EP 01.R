# Fijar carpeta de trabajo.
setwd("C:/Users/aylin/OneDrive/Desktop/IME-PEP1/Prácticos/Prácticos/EP 01")

# Importar paquetes.
library(dplyr)
library(tidyr)
library(readr)
# Cargar datos.
datos <- read.csv2("casos.csv")

# ¿Qué variables se han cargado?
# Se han cargado 735 variables, donde la primera corresponde a la región del
# país (o al total nacional) y las restantes, a la cantidad de nuevos casos de
# Covid-19 registrados cada día entre el 3 de marzo de 2020 y el 6 de marzo de
# 2022.


# ¿Qué tipo tiene cada una de estas variables?
# La primera variable (Region) es de tipo string, mientras que las restantes
# son de tipo entero.


# ¿Qué escala parecen tener estas variables?
# Las variables parecen tener una escala entera no negativa.

#¿Qué día se produjo el mayor número de casos con síntomas en la región de Los Ríos 
#entre el 01-jun-2020 y el 31-dic-2020?

# Seleccionar datos del periodo y región solicitados.
datos <- datos %>% filter(Region == "Los Ríos")
datos <- datos %>% select(X01.06.2020:X31.12.2020)

# Reordenar datos como una matriz de datos con las columnas:
# dia, mes, agno, casos.
fecha <- as.Date(colnames(datos), tryFormats = c("X%d.%m.%y"))
casos <- as.integer(t(datos))
datos <- data.frame(fecha, casos)
datos <- datos %>% separate(fecha, sep="-", into = c("agno", "mes", "dia"),
                            convert = TRUE)

datos <- datos %>% select(dia, mes, agno, casos)

# Encontrar fecha con máximo de contagios.
print("La mayor cantidad de nuevos casos sintomáticos en la región de Los Ríos entre las fechas se produjo el:")
print(datos %>% filter(casos == max(datos[["casos"]])))

# La mayor cantidad de nuevos casos sintomáticos en la región de Los Ríos entre
# el 01-junio-2020 y el 31-dic-2020 se produjo el 29-dic-2020, con 95 nuevos
# casos.


# ¿Cuál fue el total de casos con síntomas para cada mes de este periodo?

print(datos %>% group_by(mes) %>%  summarise(casos = sum(casos)))

# Junio :    244 casos
# Julio :    140 casos
# Agosto  :   136 casos
# Septiembre : 447 casos
# Octubre : 862 casos
# Noviembre: 1309 casos
# Diciembre: 1539 casos