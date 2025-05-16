# ---------------------------------------------------------------------------

# Y = Performance
# X1 = Stregth
# X1 = Stregth
# X2 = Skills
# X3 = Speed

# Yi = B0 + B1Xi1 + B2Xi2 + B3Xi3 + errores

# ---------------------------
#       PRIMER PUNTO
# ---------------------------

datos = read.csv(file.choose())

# Caracteriza la información
Y <- datos$Performance
X1 <- datos$Strength; X2 <- datos$Skills; X3 <- datos$Speed

# Ajustar modelo de regresión
modelo <- lm(Y ~ X1 + X2 + X3)

# Consultar valores ajustados
summary(modelo)

# Perf_gorro_i = 70.80 + 0.88Xi1 + 1.87Xi2 + 3.04Xi3

# ---------------------------
#       SEGUNDO PUNTO
# ---------------------------

# H0: B1 = B2 = B3 = 0
# Ha: Algún Bj != 0; j = 1,2,3

# MF: Yi = B0 + B1Xi1 + B2Xi2 + B3Xi3 + error
# MR: Yi = B0 + error

# F = ((SSR(MF) - SSR(MR))/v)/MSE(MF) = ((SSE(MR) - SSE(MF))/v)/MSE(MF)


# V = # parámetros en hipótesis nula
n <- nrow(datos); p <- length((modelo$coefficients))

#  Primer paso: ajustar el modelo completo
modelo <- lm(Y ~ X1 + X2 + X3)

# Segundo paso: ajustar el modelo reducido
modelo_reducido <- lm(Y ~ 1)

# Tercer paso: hallar antidades
# Utilizar función ANOVA

anova(modelo_reducido)
# SSE(MR) = 2183751
SSE_reducido <- 2183751

anova(modelo)
# SSE(MF) = 51427
SSE_completo <- 51427

MSE <- SSE_completo/(n-p)
F_modelo <- ((SSE_reducido - SSE_completo)/v)/MSE

# UTILIZANDO TRUCO
# Primer paso: ajustar el modelo completo
modelo <- lm(Y ~ X1 + X2 + X3)

# Segundo paso: ajustar el modelo reducido
modelo_reducido <- lm(Y ~ 1)

# Tercer paso: Utilizar función ANOVA compuesta
anova_comparacion <- anova(modelo_reducido,modelo)

# F: 6855.3
# p = 2.2e-16

# A un nivel de significancia del 5% se puede establecer que el
# modelo es significativo: Al menos un Bj es diferente ed 0/significativo

# También se puede haer con la prueba lineal general

# H0: B1 = B2 = B3 = 0
# Ha: Algún Bj != 0; j = 1,2,3

# H0: B1 = 0      H1: Algún Bj != 0;j =1,2,3
#     B2 = 0  vs 
#     B3 = 0

# LB = 0: | 0 1 0 0 | | B0 |   | 0 |
#         | 0 0 1 0 | | B1 | = | 0 |
#         | 0 0 0 1 | | B2 |   | 0 |
#                     | B3 |

# r = 3; F = (SSH/r)/MSE


# ---------------------------
#       TERCER PUNTO
# ---------------------------
# H0: Bj = 0; j = 0,1,2,3
# Ha: Bj != 0

summary(modelo)

# A un nivel de significancia del 5% se puede establecer que el intercepto,
# es significativo en presencia de las demás covariables

# A un nivel de significancia del 5% se puede establecer que el efecto de la 
# fuerza es significativo (B1 es signifiativo/!= 0) en presencia de las demás 
# covariables

# A un nivel de significancia del 5% se puede establecer que el efecto de la 
# skills es significativo (B2 es signifiativo/!= 0) en presencia de las demás 
# covariables

# A un nivel de significancia del 5% se puede establecer que el efecto de la 
# velocidad es significativo (B3 es signifiativo/!= 0) en presencia de las 
# demás covariables

# Intervalo ed confianza = Bj_gorro +- ta/2,n-p * sqrt(var[Bj_gorro])

# Construcción de un intervalo de confianza
confint(modelo)

# Con una confianza del 95% s puede establecer que el efecto promedio de la 
# fuerza está comprendido entre 0.82 y 0.94 unidades, es decir, por unidad de 
# cambio en la fuerza, el rendimiento promedio varía entre 0.82 y 0.94 
# unidades

# Si el intervalo de confianza contiene el 0, entonces, el parámetro no es 
# significativo


# ---------------------------
#       CUARTO PUNTO
# ---------------------------
# H0: B1 = B3  vs  H1: B1 != B3
#     B1 = B2          B1 != B2

# H0: B1 - B3 = 0  vs  B1 != B3
#     B1 - B2 = 0      B1 != B2

# LB = 0 -> | 0 1 0 -1 | | B0 |   | 0 |  r = 2
#           | 0 1 -1 0 | | B1 | = | 0 |
#                        | B2 |
#                        | B3 |

# MF = Yi = B0 + B1Xi1 + B2Xi2 + B3Xi3 + error
# MR = Yi = B0 + B1(Xi1 + Xi2 + Xi3)+ error

# Primer paso: ajustar el modelo completo
modelo <- lm(Y ~ X1 + X2 + X3)

# Segundo paso: ajustar el modelo reducido
X123 <- X1 + X2 + X3
modelo_LG <- lm(Y ~ X123)

# Tercer paso: aplicar el truco
anova_LG <- anova(modelo_LG, modelo)

# A un nivel de sigmificancia del 5% se puede rechazar la hipótesis nula, así,
# se puede restablecer que el efecto de l fuerza es distinto de la velocidad, 
# a su vez,el efecto de la primera covriable es diferente al efecto de las 
# habilidades




















