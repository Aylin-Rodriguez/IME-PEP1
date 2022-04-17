#GRUPO 7 - Actividad Practica 1
#AUTORES: AYLIN RODRIGUEZ - IGNACIO VILLARROEL
#ACTIVIDADES.
#                        ¿Qué variables se han cargado?
#RESPUESTA: Las varaibles que se cargaron fueron las siguientes: 
#     Region: que corresponde al nombre de la region,
#     Casos: corresponde a la cantidad de contagiados con sintomas
#     Fecha: es la fecha donde se registrÃ³ el numeros de Casos en el dia.


#                   ¿Qué tipo tiene cada una de estas variables?
#RESPUESTA: La variable Region es del tipo Categórica nominal,
#           La variable Casos es del tipo Numérica discreta y
#           La variable Fecha es del tipo Categórica ordinal.

#                  ¿Qué escala parecen tener estas variables?
#RESPUESTA:Region tiene una escala nominal, Casos tiene una escala de razón y Fecha tiene una escala ordinal.
library(dplyr)
library(tidyr)
library(readr)

######Ejercicio 1#######
#¿Qué día se produjo el mayor número de casos con síntomas en la región de Los Ríos 
#entre el 01-jun-2020 y el 31-dic-2020?
#Respuesta:
datos <- read.csv2("C:\\Users\\aylin\\Downloads\\casos.csv")
#Se filtran los datos de la región solicitada
losrios <- filter(datos, Region == "Los Ríos")
cabeza <- names(losrios)[-1]
datosLargos <- losrios %>% pivot_longer(cabeza, names_to = "Fechas", values_to = "Casos", values_transform  = as.integer)
#Se obtiene el rango de fechas solicitado en el formato "xDía.Mes.Año"
rangoFechas <- "X01.06.2020" <= cabeza & "X31.12.2020" >= cabeza
final_fechas <- cabeza[rangoFechas]
final_casos <- datosLargos[rangoFechas,"Casos"]
#Se obtiene el máx de contagios
maximoContagios <- which.max(final_casos[["Casos"]])
print("El día con mas contagios es:")
print(final_fechas[maximoContagios])

######Ejercicio 2#######
#¿Cuál fue el total de casos con síntomas para cada mes de este periodo?
#Respuesta: 
#Se crea una tabla filtrando los casos por cada mes
losriosjunio <- datosLargos %>% filter("X01.06.2020" <= Fechas & "X30.06.2020" >= Fechas)
losriosjulio <- datosLargos %>% filter("X01.07.2020" <= Fechas & "X31.07.2020" >= Fechas)
losriosagosto <- datosLargos %>% filter("X01.08.2020" <= Fechas & "X31.08.2020" >= Fechas)
losriossept <- datosLargos %>% filter("X01.09.2020" <= Fechas & "X30.09.2020" >= Fechas)
losriosoct <- datosLargos %>% filter("X01.10.2020" <= Fechas & "X31.10.2020" >= cabeza)
losriosnov <- datosLargos %>% filter("X01.11.2020" <= Fechas & "X30.11.2020" >= cabeza)
losriosdic <- datosLargos %>% filter("X01.12.2020" <= Fechas & "X31.12.2020" >= cabeza)

#Se calculan los casos de contagio por mes y el máximo de casos del periodod junio-diciembre
informacion <- datosLargos %>% summarise(junio = sum(losriosjunio$Casos),
                                         julio = sum(losriosjulio$Casos),
                                         agosto = sum(losriosagosto$Casos),
                                         sept = sum(losriossept$Casos),
                                         oct = sum(losriosoct$Casos),
                                         nov = sum(losriosnov$Casos),
                                         dic = sum(losriosdic$Casos),
)
print("El total de casos con síntomas para cada mes entre el periodo junio-diciembre fue:")
print(informacion)