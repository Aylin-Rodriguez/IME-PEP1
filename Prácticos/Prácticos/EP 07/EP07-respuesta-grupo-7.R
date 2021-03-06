############# PR�CTICO 7 #############
#Alumnos: Mat�as Bozo - Aylin Rodriguez - Ignacio Villarroel.

library(tidyr)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(pwr)
library(magrittr)
library(tidyverse)
library(RVAideMemoire)
library(rcompanion)

#datos <- read.csv2("C:\\Users\\aylin\\Downloads\\EP07 Datos.csv")
datos4 <- read.csv2(file.choose(), stringsAsFactors = FALSE, encoding = "UTF-8")

############################  PREGUNTA 1  ########################################
# 1.En su desquiciada investigaci�n para acabar con los vampiros, Van Helsing 
# ha descubierto que sus enemigos tienen predilecci�n por la sangre humana tipo 
# AII+. El cazador sospecha que estos monstruos tienen preferencia por la sangre
# de los adultos, pero a�n no est� convencido. Por esta raz�n, mediante artima�as, 
# ha encerrado a 14 ni�os y 20 adultos con este tipo de sangre en un reducto de 
# vampiros. Tras 3 noches, 4 de los ni�os y 11 de los adultos fueron atacados. 
# �Existe relaci�n entre el ataque de vampiros y la edad de la v�ctima?
##################################################################################
#               PRUEBA DE CHI-CUADRADO DE INDEPENDENCIA
##################################################################################

#H0: No existe una relaci�n entre el ataque de vampiros y la edad de la v�ctima.
#H1: Existe una relaci�n entre el ataque de vampiros y la edad de la v�ctima.

#Crear tabla de contingencia.
ninos <- c(10, 4)
adultos <- c(9, 11)

tabla1 <- as.table(rbind(ninos, adultos))

dimnames(tabla1) <- list(c("Atacado", "No Atacado"),
                        c("Ni�os", "Adultos"))

print(tabla1)               

# Hacer prueba chi-cuadrado de independencia.
prueba1 <- chisq.test(tabla1)
cat("\nLa prueba internamente calcula los valores esperados :\n")
esperados <- round(prueba1[["expected"]], 0)
print(esperados)
cat("\nResultado de la prueba :\n")
print(prueba1) 

############################  PREGUNTA 2  ########################################
# Una Universidad ha detectado que muchos de sus nuevos estudiantes ingresan con 
# elevados niveles de ansiedad. Para ello, han decidido evaluar un nuevo programa 
# de bienvenida que busca facilitar la adaptaci�n a la vida universitaria. Para 
# ello, han reclutado a un grupo de 15 voluntarios a quienes se les midi� el nivel 
# de ansiedad (alto o bajo) antes y despu�s de participar en el programa de 
# bienvenida:
# - 2 estudiantes no presentaron ansiedad ni antes ni despu�s.
# - 9 estudiantes inicialmente ansiosos dejaron de estarlo.
# - 3 estudiantes mantuvieron un elevado nivel de ansiedad.
# - El estudiante restante desarroll� s�ntomas de ansiedad tras participar en el 
#   programa.
# �Qu� se puede concluir acerca del nuevo programa de bienvenida?
##################################################################################
#                         PRUEBA DE MCNEMAR
##################################################################################

# H0: no hay cambios significativos posterior a la bienvenida.
# H1: s� hay cambios significativos posterior a la bienvenida.

# Se utiliza la prueba mcnemar ya que como conclusi�n al programa de bienvenida,
#se pretende saber si este realiza un cambio significativo, adem�s nos permite
#mostrar las frecuencia mediante una matriz

# Construir la tabla de contingencia.
estudiantes <- seq(1:15)
antes <- c(rep("alto", 12), rep("bajo", 3))
despues <- c(rep("alto", 4), rep("bajo", 11))
datos <- data.frame(estudiantes, antes, despues)
tabla2 <- table(despues, antes)
print(tabla2)

# Aplicar prueba de McNemar.
prueba2 <- mcnemar.test(tabla2)
print(prueba2)  


############################  PREGUNTA 3  ########################################
# En noviembre de 2019, se realiz� un estudio acerca de la aprobaci�n al 
# presidente Sebasti�n Pi�era entre 440 profesores y estudiantes de una 
# prestigiosa universidad, obteni�ndose los resultados que se muestran en 
# la tabla. �Son similares las opiniones de ambos segmentos de la comunidad 
# universitaria?
#                      Aprueba Desaprueba Ninguna Total
# Estudiantes            35       208       17     260
# Profesores             20       157        3     180
# Total                  55       365       20     440

#H0: Son similares la opiniones de los segmentos de la comunidad universitaria.
#H1: No son similares la opiniones de los segmentos de la comunidad universitaria.

#Esta prueba se utiliza porque se busca determinar si las dos poblaciones presentan las mismas proporciones 
# en sus diferentes niveles. Adem�s se encarga de encontrar la proporcionalidad (nivel de similutd), que en este caso,
# es lo que se est� preguntando.
# 
##################################################################################
#                         
##################################################################################

#Construir tabla de contingencia.
estudiantes <- c(35, 208, 17)
profesores <- c(20, 157, 3)

tabla3 <- as.table(rbind(estudiantes, profesores))

dimnames(tabla3) <- list(persona = c("estudiantes", "profesor"),
                          opinion = c("aprueba", "desaprueba", "ninguna"))
print(tabla3)

#Se realiza la prueba la prueba chi-cuadrado de independencia.

prueba3 <- chisq.test(tabla3)
print (prueba3)

#Debido a que Existe evidencia suficiente con 95% de confianza de que no hay similitud en las opiniones de 
# los segmentos de la comunidad universitaria
############################  PREGUNTA 4  ########################################
# La Facultad de Ingenier�a desea saber si existe diferencia significativa 
# en el desempe�o de los estudiantes en asignaturas cr�ticas de primer semestre. 
# Para ello, le ha entregado un archivo de datos que, para 3 asignaturas, indica 
# si una muestra de 50 estudiantes aprob� o reprob�. �Qu� puede concluir la 
# Facultad? Indicaci�n: obtenga la muestra a partir del archivo EP07 Datos.csv, 
# usando la semilla 555. Considere un nivel de significaci�n ??=0,05.
##################################################################################
#                         PRUEBA Q DE COCHRAN
##################################################################################

# H0: la proporci�n del desempe�o es la misma para todas las materias.
# HA: la proporci�n del desempe�o es distinta para al menos una materia.

# Para esta pregunta se utiliza la prueba de Cochran pues, es la m�s adecuada cuando 
# la variable de respuesta es dicot�mica y la variable independiente tiene m�s de dos 
# observaciones pareadas y sirve para verificar que tratamientos tienen efectos id�nticos.

# Se debe tomar una muestra de 50 estudiantes para realizar la prueba.

# Crear matriz de datos.
instancia <- 1:1500
calculo <- datos4 %>% select("Calculo")
algebra <- datos4 %>% select("Algebra")
fisica <- datos4 %>% select("Fisica")
datos4 <- data.frame(instancia, calculo, algebra, fisica)

# Llevar matriz de datos a formato largo.
datos4 <- datos4 %>% pivot_longer(c("Calculo", "Algebra", "Fisica"),
                                  names_to = "metaheuristica",
                                  values_to = "resultado")

datos4[["instancia"]] <- factor(datos4[["instancia"]])
datos4[["metaheuristica"]] <- factor(datos4[["metaheuristica"]])

# Hacer prueba Q de Cochran.
prueba4 <- cochran.qtest(resultado ~ metaheuristica | instancia,
                          data = datos4, alpha = 0.05)

print(prueba4)

# Procedimiento post -hoc con correcci �n de Bonferroni.
post_hoc_1 <- pairwiseMcnemar(resultado ~ metaheuristica | instancia,
                                      data = datos4, method = "bonferroni")

cat("\nProcedimiento post-hoc con correcci�n de Bonferroni\n")
print(post_hoc_1)


# Procedimiento post-hoc con correcci�n de Holm.
post_hoc_2 <- pairwiseMcnemar(resultado ~ metaheuristica | instancia,
                                      data = datos4, method = "holm")

cat ("\nProcedimiento post-hoc con correcci�n de Holm\n")
print(post_hoc_2)

# Se tiene que el valor p , menor que el nivel de significaci�n ?? = 0, 05, 
# por lo que rechazamos la hip�tesis nula en favor de la hip�tesis alternativa. 
# En consecuencia, se concluye con 95 % de confianza que al menos una de las 
# metaheur�sticas tiene un desempe�o diferente a las dem�s.
