############# PR�CTICO 14 #############
#Alumnos: Matias Bozo - Aylin Rodriguez - Ignacio Villarroel.
library(pROC)
library(caret)
library(dplyr)
library(car)
library(ggpubr)

# Lectura del archivo
datos <- read.csv2(file.choose(), stringsAsFactors = FALSE)

# Se agrega la columna IMC
datos[["IMC"]] <- datos[["Weight"]] / ((datos[["Height"]] / 100)^2) 

#Crear la variable dicot�mica
# 1: Sobrepeso
# 0: No sobrepeso
condicion <- ifelse(datos[["IMC"]] >= 25, 1,  0)
# Se transforma EN en variable categ�rica
# EN: Estado Nutricional
datos[["EN"]] <- factor(condicion)

# RUT del integrante mayor: 18954417-0
# 1.- Definir semilla a utilizar
seed <- 4417 
set.seed(seed)

# Como la semilla es un n�mero impar se obtienen s�lo los hombres
hombres <- datos %>% filter(Gender == "1")

# 2.- Se seleccionan 25 datos de sobrepeso y 25 de no sobrepeso debido a un problema de datos que se tuvo al momento de 
# realizar el modelo de regresi�n log�stica, ya que en la muestra hab�a muchas observaciones con EN = 0.
sobrepeso <- hombres %>% filter(EN == 1)
muestraSobrepeso <- sobrepeso[sample(nrow(sobrepeso), 25),]

noSobrepeso <- hombres %>% filter(EN == 0)
muestraNoSobrepeso <- noSobrepeso[sample(nrow(noSobrepeso), 25),]

datos <- rbind(muestraNoSobrepeso,muestraSobrepeso)


# 3.- Recordando las 8 variables predictoras del ejercicio anterior:
# "Thigh.Girth"    "Calf.Maximum.Girth"   "Shoulder.Girth"  "Knee.Girth"
# "Waist.Girth"    "Bitrochanteric.diameter"      "Age"     "Ankle.Minimum.Girth"

# 4.- La variable seleccionada es: 
# Chest.Girth (circunferencia del pecho), ya que al tener una circunferencia del pecho
# mayor o menor, se podr?a suponer que esto tambi?n influye en el peso de la persona,
# en otras palabras, al tener mayor o menor circunferencia, se tiene mayor o menor volumen
# corporal respectivamente, lo que influye en su peso.

# 5.- Se construye el modelo de regresi�n log�stica con el predictor "Chest.diameter" 

# Separar conjuntos de entrenamiento y prueba
n <- nrow(datos)
n_entrenamiento <- floor(0.8 * n)
muestra <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)
entrenamiento <- datos[muestra, ]
prueba <- datos[-muestra, ]

# Ajustar modelo de regresi�n log�stica
modelo <- glm (EN ~ Chest.diameter, family = binomial(link = "logit"), data = entrenamiento)
print(summary(modelo))

# Evaluar el modelo con el conjunto de entrenamiento
cat ("Evaluaci�n del modelo a partir del conjunto de entrenamiento :\n")
probs_e <- predict(modelo, entrenamiento, type = "response")

umbral <- 0.5
preds_e <- sapply(probs_e , function (p) ifelse( p >= umbral , "1", "0"))
preds_e <- factor ( preds_e , levels = levels ( datos [["EN"]]) )

ROC_e <- roc(entrenamiento[["EN"]], probs_e)
plot(ROC_e)

matriz_e <- confusionMatrix(preds_e , entrenamiento [["EN"]])
print(matriz_e)

# Evaluar el modelo con el conjunto de prueba.
cat (" Evaluaci�n del modelo a partir del conjunto de prueba :\n")
probs_p <- predict(modelo, prueba , type = "response")

preds_p <- sapply(probs_p , function (p) ifelse ( p >= umbral , "1", "0") )
preds_p <- factor(preds_p , levels = levels ( datos [["EN"]]) )

ROC_p <- roc(prueba[["EN"]] , probs_p)
plot(ROC_p)

matriz_p <-confusionMatrix(preds_p , prueba[["EN"]])
print(matriz_p)

# ------ AN�LISIS DEL MODELO Y MATRIZ DE CONFUSI�N ------
# Al analizar el modelo podemos apreciar que el valor de AIC es 53.315, y que la desviaci�n 
# del modelo con una variable (38 grados de libertad) es de 49.315.

# La curva de ROC de prueba se aleja de la diagonal, por lo que al parecer se trata de un buen modelo.

# Si nos fijamos en los valores obtenidos en la matriz de confusi�n para los datos de entrenamiento podemos observar que se 
# obtiene una exactitud de 62,5% , una sensibilidad de 66,67% y la especificidad de 57,89% 

# Para el caso del conjunto de prueba se tiene una exactitud de 70%, una sensibilidad de 75%, y la especificidad de 
# 66,67%. 

# 6.- Usando herramientas para la exploraci�n de modelos del entorno R, buscar entre dos y cinco
# predictores de entre las variables seleccionadas al azar, recordadas en el punto 3, para agregar al
# modelo obtenido en el paso 3

# La variables seleccionadas son:
# - Thigh.Girth
# - Shoulder.Girth

# Ahora se agrega cada una de las variables seleccionadas al modelo
modelo_RLM <- update (modelo, . ~ . + Thigh.Girth)
modelo_RLM <- update (modelo_RLM, . ~ . + Shoulder.Girth)
print(summary(modelo_RLM))

# ----------- EVALUACI�N DEL MODELO RLM -----------
# Obtener los residuos y las estad�sticas .
output <- data.frame (predicted.probabilities = fitted(modelo_RLM))
output [["standardized.residuals"]] <- rstandard(modelo_RLM)
output [["studentized.residuals"]] <- rstudent( modelo_RLM )
output [["cooks.distance"]] <- cooks.distance(modelo_RLM)
output [["dfbeta"]] <- dfbeta(modelo_RLM )
output [["dffit"]] <- dffits(modelo_RLM)
output [["leverage"]] <- hatvalues(modelo_RLM)

# Evaluar residuos estandarizados que escapen a la normalidad.
# 95 % de los residuos estandarizados deber�an estar entre
# -1.96 y 1.96 , y 99 % entre -2.58 y 2.58.
sospechosos1 <- which (abs(output[["standardized.residuals"]]) > 1.96)
sospechosos1 <- sort(sospechosos1 )
cat ("\n\n")
cat (" Residuos estandarizados fuera del 95 % esperado \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - -\n")
print(rownames(entrenamiento[sospechosos1, ]) )

# Revisar casos con distancia de Cook mayor a uno.
sospechosos2 <- which(output[["cooks.distance"]] > 1)
sospechosos2 <- sort(sospechosos2)
cat ("\n\n")
cat ("Residuales con una distancia de Cook alta \n")
cat ("- - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - -\n")
print(rownames(entrenamiento[sospechosos2, ]))

# Revisar casos cuyo apalancamiento sea m�s del doble
# o triple del apalancamiento promedio .
leverage.promedio <- ncol(entrenamiento)/nrow(datos)
sospechosos3 <- which(output [["leverage "]] > leverage.promedio)
sospechosos3 <- sort(sospechosos3)

cat ("\n\n")

cat (" Residuales con levarage fuera de rango ( > ")
cat (round(leverage.promedio, 3) , ")", "\n", sep = "")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print(rownames(entrenamiento[sospechosos3, ]) )

# Revisar casos con DFBeta >= 1.
sospechosos4 <- which(apply(output[["dfbeta"]] >= 1 ,1 ,any))
sospechosos4 <- sort(sospechosos4)
names(sospechosos4 ) <- NULL
cat ("\n\n")
cat (" Residuales con DFBeta sobre 1\n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - -\n")
print(rownames(entrenamiento[sospechosos4 , ]))

# Detalle de las observaciones posiblemente at� picas .
sospechosos <- c(sospechosos1, sospechosos2, sospechosos3, sospechosos4)
sospechosos <- sort (unique(sospechosos))
cat ("\n\n")
cat (" Casos sospechosos \n")
cat (" - - - - - - - - - - -- - - - - -\n")
print(entrenamiento[sospechosos, ])
cat("\n\n")
print(output[sospechosos , ])


# -------------- VERIFICACI�N DE CONDICIONES ----------

# Verificaci�n de multicolinealidad .
cat ("Verificaci�n de colinealidad \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
cat ("\n VIF :\n")
vifs <- vif (modelo_RLM)
print ( vifs )
cat ("\n Promedio VIF: ")
print ( mean ( vifs ) )
# Si miramos los factores de inflaci�n de la varianza, en general no parecen ser preocupantes, por lo que se verifica 
# la condici�n de multicolinealidad.

# Independencia de los residuos.
cat (" Verificaci�n de independencia de los residuos \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print(durbinWatsonTest(modelo_RLM) )

# Como se obtiene un p-value = 0.388 > alfa = 0.05, se puede decir que se cumple la condici�n de independencia de los residuos.
# Tambi�n como se puede observar, el promedio del VIF es 1.379261, lo cual est� cercano a 1 y no genera grandes problemas.
# por lo que se considera no ajustar el modelo.

# ----------- EVALUAR PODER PREDICTIVO PARA RLM -------
# Evaluar el modelo con el conjunto de entrenamiento
cat ("Evaluaci�n del modelo a partir del conjunto de entrenamiento :\n")
probs_e <- predict(modelo_RLM, entrenamiento, type = "response")

umbral <- 0.5
preds_e <- sapply(probs_e , function (p) ifelse( p >= umbral , "1", "0"))
preds_e <- factor ( preds_e , levels = levels ( datos [["EN"]]) )

ROC_e <- roc(entrenamiento[["EN"]], probs_e)
plot(ROC_e)

matriz_e <- confusionMatrix(preds_e , entrenamiento [["EN"]])
print(matriz_e)

# Evaluar el modelo con el conjunto de prueba.
cat (" Evaluaci�n del modelo a partir del conjunto de prueba :\n")
probs_p <- predict(modelo_RLM, prueba , type = "response")

preds_p <- sapply(probs_p , function (p) ifelse ( p >= umbral , "1", "0") )
preds_p <- factor(preds_p , levels = levels ( datos [["EN"]]) )

ROC_p <- roc(prueba[["EN"]] , probs_p)
plot(ROC_p)

matriz_p <-confusionMatrix(preds_p , prueba[["EN"]])
print(matriz_p)

# Al observar la matriz de confusi�n del modelo que utiliza 3 variables predictivas (modeloRLM), se puede observar que tanto 
# su exactitud, como la sensibilidad y la especificidad es mucho mejor que el modelo donde se utiliza s�lo una variable (modelo).

# Adem�s, al observar los gr�ficos de la curva de ROC para los grupos de entrenamiento y de prueba, �stas se alejan mucho
# m�s de la diagonal, por lo que al parecer se trata de un buen modelo..


#----------------------------- COMPARANDO LOS MODELOS --------------------------------------

# Hip�tesis a contrastar

# H0: Ambos modelos son iguales
# HA: Un modelo es mejor que otra

# Comparar los modelos RLS y el RLM
cat ("\n\n")
cat (" Likelihood Ratio Test para los modelos \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print (anova(modelo, modelo_RLM, test = "LRT") )

# Observando los resultados obtenidos al comparar los dos modelos, podemos decir que el modelo_RLM (3 predictores) 
# es mejor que el modelo simple (1 predictor), debido a que tiene su p-valor = 4.572e-05, 
# el cual es muy significativo.