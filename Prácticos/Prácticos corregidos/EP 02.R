# Fijar carpeta de trabajo.
setwd("C:/Users/aylin/OneDrive/Desktop/IME-PEP1/Prácticos/Prácticos/EP 02")

# Fijar una semilla para asegurar que los resultados sean reproducibles.
set.seed(87)

# Importar paquetes.
library(dplyr)
library(ggpubr)

# Cargar datos.
datos <- read.csv2("datoscasen.csv", stringsAsFactors = TRUE)

# Obtener una muestra de 150 observaciones.
n_muestra <- 150
muestra <- sample_n(datos, n_muestra)

# Si reescalamos la variable ytot de $1 a $1.000, tendremos números más claros.
datos[["ytot"]] <- datos[["ytot"]] / 1000
muestra[["ytot"]] <- muestra[["ytot"]] / 1000


################################################################################
# ANÁLISIS DE DOS VARIABLES NUMÉRICAS
# ¿Tiene relación el ingreso con la riqueza del municipio donde se habita?
################################################################################

cat("\n¿Tiene relación el ingreso con la riqueza del municipio donde se ")
cat("habita?\n")

# En este caso, queremos ver si una variable numérica influye sobre otra
# variable (también numérica):
# - Si una variable crece, ¿la otra también?
# - O bien, si una bariable crece, ¿disminuye la otra?
# - O tal vez no hay relación, es decir, las variables son independientes.

# Una herramienta adecuada para abordar este tipo de preguntas es el gráfico de
# dispersión.

# Como siempre, comencemos por estudiar la muestra.
# Crear gráfico de dispersión.
g6_1_m <- ggscatter(muestra, x = "ing.comuna", y = "ytot", color = "#6D9EC1",
                    fill = "#BFD5E3",
                    title = "Relación ingreso - riqueza de la comuna en la RM",
                    subtitle = "Muestra",
                    ylab = "Ingreso total (miles de pesos)",
                    xlab = "Ranking de riqueza (ascendente)")

g6_1_m <- g6_1_m + scale_y_continuous(limits = c(0, 5050))
print(g6_1_m)

# Parecería que hay una relación positiva, aunque leve.

# Agregamos una línea de tendencia para ver la relación de forma más clara.
g6_2_m <- ggscatter(muestra, x = "ing.comuna", y = "ytot", add = "reg.line",
                    add.params = list(color = "#FC4E07"), color = "#6D9EC1",
                    fill = "#BFD5E3",
                    title = "Relación ingreso - riqueza de la comuna en la RM",
                    subtitle = "Muestra",
                    ylab = "Ingreso total (miles de pesos)",
                    xlab = "Ranking de riqueza (ascendente)")

g6_2_m <- g6_2_m + scale_y_continuous(limits = c(0, 5050))
print(g6_2_m)

# Ahora sí es claro que si una variable crece, la otra también lo hace. Aunque
# la relación no parece muy fuerte.

# Veamos ahora qué pasa con la población.
g6_2_p <- ggscatter(datos, x = "ing.comuna", y = "ytot", add = "reg.line",
                    add.params = list(color = "#FC4E07"), color = "#6D9EC1",
                    fill = "#BFD5E3",
                    title = "Relación ingreso - riqueza de la comuna en la RM",
                    subtitle = "Población",
                    ylab = "Ingreso total (miles de pesos)",
                    xlab = "Ranking de riqueza (ascendente)")

g6_2_p <- g6_2_p + scale_y_continuous(limits = c(0, 5050))
print(g6_2_p)

# En efecto, hay una relación leve.

cat("\n")
cat("=========================================================================")
cat("\n")


