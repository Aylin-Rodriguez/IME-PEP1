#GRUPO 7 - Actividad Practica 2
#AUTORES: AYLIN RODRIGUEZ - IGNACIO VILLARROEL

library(dplyr)
library(tidyr)
library(ggpubr)
setwd("C:/Users/aylin/OneDrive/Desktop/IME-PEP1/Prácticos/Prácticos/EP 02")
datoscasen <- read.csv2("datoscasen.csv", stringsAsFactors = TRUE)

mujer <- datoscasen %>% filter(sexo == "Mujer")

agrupar <- group_by(mujer, comuna) %>% summarise(mean(ytot))

#casen <- as.data.frame(xtabs(~ comuna, data = agrupar))

g <- ggbarplot(agrupar,
               
              x = "comuna",
              y = "mean(ytot)",
              fill = c("brown", "purple", "orange", "blue", "brown", "purple", "orange", "blue",
                       "brown", "purple", "orange", "blue","brown", "purple", "orange", "blue",
                       "brown", "purple", "orange", "blue","brown", "purple", "orange", "blue",
                       "brown", "purple", "orange", "blue","brown", "purple", "orange", "blue",
                       "brown", "purple", "orange", "blue","brown", "purple", "orange", "blue",
                       "brown", "purple", "orange", "blue","brown", "purple", "orange", "blue",
                       "brown", "purple", "orange", "blue") ,
              title = "Ingreso promedio de mujeres por municipio",
              xlab = "Comunas",
              ylab = "Ingreso promedio",
              width = 1,
              orientation = "horiz"
              )
print(g)

#PREGUNTA GRUPO 7

# ?Tiene relaci?n el ingreso de las mujeres de la RM con la riqueza del municipio donde habita?

# RESPUESTA: Como se puede notar en el gr?fico, las mujeres de Vitacura, Providencia, Las Condes, ?u?oa y la Reina,
# poseen mayores ingresos, esto no concuerda del todo ya existen municipios como Maipu que tienen una alta riqueza,
# sin embargo el ingreso promedio de mujeres de esa comuna no refleja la riqueza del municipio.

# Fuente Riqueza Municipios: https://www.latercera.com/nacional/noticia/diferencias-8-veces-registran-comunas-rm-gasto-per-capita/103389/
