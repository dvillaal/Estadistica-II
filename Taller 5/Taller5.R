# ---------------------------------------
#            PRIMER PUNTO
# ---------------------------------------
datos <- read.csv(file.choose())
modelo <- lm(price ~ bodyStyle * bore, data = datos)

summary(modelo)

# Realizar prueba de hipótesis intercepto

library(car)
linearHypothesis(modelo, "bodyStylesedan=0")
linearHypothesis(modelo, "bodyStylesedan:bore=bodyStylewagon:bore")
 
datos2 <- read.csv(file.choose())

source("funciones.R")
myBackward(datos2[, -5])