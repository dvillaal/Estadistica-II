# ---------------------------------------------------------------------------
# -------------------------
#       PRIMER PUNTO
# -------------------------

datos <- read.csv(file.choose())
Y <- datos$Performance
X1 <- datos$Strength; X2 <- datos$Skills; X3 <- datos$Speed
modelo <- lm(Y ~ X1 + X2 + X3)

# Yi = B0 + B1Xi1 + B2Xi2 + B3Xi3 + error

# Verifiar normalidad
# Método gráfico
plot(modelo, 2)

# Criterio analítico: Shapiro-Wilks
# H0: Ei ~ Normal
# H1: Ei != Normal
shapiro.test(modelo$residuals)
# valor p = 0.1877 > a = 0.05. Por lo tanto, NO se rechaza hipótesis nula: Se
# puede concluir a un nivel de significancia del 5%, los errores tienen una 
# distribución ~ normal

# Verificación de varianza constante
plot(modelo, 1)

# Si se cumple la homogeneidad de varianza

# Verificación de media 0. Siempre se cumple

# -------------------------
#       SEGUNDO PUNTO
# -------------------------

# Puntos atípicos: |di| > 3; |ri| > 3
# Puntos de balanceo: hii > 2p/n : 2p/n < 1
# p: # de parámetros; n: # de observaciones  
n <- nrow(datos); p <- 4

hat_values <- hatvalues(modelo) # Para puntos de balanceo hii
balanceo <- which(hat_values > ((2 * p)/n)) # Filtra los datos

# 75 102 194 274 311: son puntos de balanceo

# puntos de balanceo: Pueden afectar R^2; y la estimación del error estándar 
# asociado Bj_gorro

estandarizados <- rstandard(modelo)
estudentizados <- rstudent(modelo)

atipicos_estandarizados <- which(abs(estandarizados) >  3)
atipicos_estudentizados <- which(abs(estudentizados) >  3)

# 16 167 219 316: son puntos atípicos

# Puntos de balanceo: Distancias horizontales
# # Puntos atípicos: Se alejen muchos verticalmente

# Crear gráfica a mano
plot(fitted(modelo), rstandard(modelo))
abline(h = -3)
abline(h = 3)

# Una observación influencial, se caracteriza como tal si es inusual en X y Y

# -------------------------
#       TERCER PUNTO
# -------------------------
cooks <- cooks.distance(modelo)
which(cooks > 1)

# No hay influenciales según cooks

DFBetas <- dfbetas(modelo)
which(abs(DFBetas) > 2/sqrt(n))
# Hay muchísimas influenciales según DFBETAS

DFFITS <- dffits(modelo)
which(abs(DFFITS) > (2 * sqrt(p/n)))
# No hay tantas según DFFITS

influencias <- influence.measures(modelo)
summary(influencias)


# -------------------------
#       CUARTO PUNTO
# -------------------------

# Punto de extrapolación: X0(X'X)^-1 X0' < max {hii}

X <- model.matrix(modelo)
Hat_values <- hat(X)

x01 <- c(1, 45.03, 80.88, 60.33)
x01 <- c(1, 77.88, 100, 13.76)
summary(X)

ifelse(t(x01)%*%solve(t(X)%*%X)%*%x01 < max(Hat_values), "Pertenece a la región de diseño", "No pertenece")
ifelse(t(x02)%*%solve(t(X)%*%X)%*%x02 < max(Hat_values), "Pertenece a la región de diseño", "No pertenece")

# x01: Se pueden realizar predicciones
# x02: No se pueden realizar predicciones


# Intervalo de predicción: Y0_gorro +- ta/2,n-p s.e(Y0-Y0_gorro)

predict(modelo, newdata = data.frame(X1 = 45.03,
                                     X2 = 80.88,
                                     X3 = 60.33), interval = "prediction")























