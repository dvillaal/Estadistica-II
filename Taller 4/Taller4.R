# ---------------------------------------
#            PRIMER PUNTO
# ---------------------------------------

datos <- read.csv(file.choose())

# Ajustar la regresión
Y <- datos$Performance
X1 <- datos$Strength; X2 <- datos$Skills; X3 <- datos$Speed

modelo <- lm(Y ~ X1 + X2 + X3)

# ---------------------------------------
 
# mirar matriz de correlción de mis datos
cor(datos[, -1])
summary(modelo)
anova(modelo)

# ---------------------------------------
#            SEGUNDO PUNTO
# ---------------------------------------
library(car)
library(olsrr)

vif(modelo)  # Identificar multicolinealidad
ols_coll_diag(modelo) # Análisis general

# ---------------------------------------
#            TERCER PUNTO
# ---------------------------------------}
source(file.choose())
library(leaps)
library(perturb)

myAllRegTable(modelo)
myCp_criterion(modelo)
myR2_criterion(modelo)
myAdj_R2_criterion(modelo)











