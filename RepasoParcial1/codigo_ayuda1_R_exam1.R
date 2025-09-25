
library(rio)
library(here)
library(magrittr)  # o library(dplyr)
datos <- rio::import(here(file.choose()))

### ver la estructura de los datos
head(datos)

#install.packages("car")
library(car)

Cantidad_02 <- datos$Cantidad_O2
X1 <- datos$X1
X2 <- datos$X2
X3 <- datos$X3
X4 <- datos$X4
X5 <- datos$X5
X6 <- datos$X6

modelo <- lm(Cantidad_02 ~ X1 + X2 + X3 + X4 + X5 + X6, data = datos)

summary(modelo)
summary(modelo)$coefficients %>% round(digits = 6)

source(file.choose())
myAnova(modelo)

modelo_R <- lm(Cantidad_02 ~ 1)

res <- anova(modelo_R, modelo)
res

source(file.choose())
myAllRegTable(modelo)

### MR PH-SUB_CONJUNTO

modelo_MR <- lm(Cantidad_02 ~ X1 + X2 + X3, data = datos)

anova(modelo_MR, modelo)

summary(modelo_MR)
summary(modelo_MR)$coefficients

source(file.choose())
myAnova(modelo_MR)

anova(modelo_MR, modelo)

### MR PHLG
## Datos para MR en PHLG
datos <- modelo$model         ## matriz de datos-xY
head(datos)

## Matriz Diseño X
X <- model.matrix(modelo)     #Residuals 27 207.06    7.67          
head(X)

datos_MR_PHLG <- data.frame(datos$Cantidad_O2,datos$X1+datos$X2,
                     datos$X3,datos$X4,datos$X5+datos$X6)

colnames(datos_MR_PHLG) <- c("Cantidad_O2","X12","X3","X4","X56")
head(datos_MR_PHLG)

## Ajuste del MR en PHLG

modelo_mr_phlg <- lm(Cantidad_O2 ~., data=datos_MR_PHLG)
source(file.choose())
myAnova(modelo_mr_phlg)


summary(modelo_mr_phlg)
summary(modelo_mr_phlg)$coefficients


## Validación de supuestos de errores

## Prueba de normalidad de errores
source(file.choose())
myQQnorm(modelo)

shapiro.test(modelo$residuals)

estudentizados <- rstudent(modelo)
atipicos_estudentizados <- which(abs(estudentizados) > 3)

## Evaluación de varianza constante de errores

## residuales-crudos
e.residuales <- modelo$residuals

# Cálculo de residuales estudentizados y valores ajustados
res.stud <- rstandard(modelo)
head(res.stud)

y_gorro <- modelo$fitted.values
head(y_gorro)

# Gráfico de Residuales crudos vs. Valores ajustados
plot(y_gorro, e.residuales, xlab = "Valores Ajustado", ylab = "Residuales Crudos",
     main = "Residuales Crudos vs. Valores Ajustados")
abline(h = 0, lty = 2, col = 2)

# Gráfico de Residuales estudentizados vs. Valores ajustados
plot(y_gorro, res.stud, xlab = "Valores Ajustado", ylab = "Residuales Estudentizados",
     main = "Residuales Estudentizados vs. Valores Ajustados")
abline(h = 0, lty = 2, col = 2)

estandarizados <- rstandard(modelo)
estudentizados <- rstudent(modelo)

atipicos_estandarizados <- which(abs(estandarizados) >  3)
atipicos_estudentizados <- which(abs(estudentizados) >  3)

at## Tabla de resumen para diagnóstico de valores extremos

## Cálculo de errores estándar de los valores ajustados
se.y_gorro <- round(predict(modelo, se.fit = T)$se.fit,4)

## Residuales crudos del modelo
ei <- round(modelo$residuals, 4)

## Distancias de Cook
Cooks.D <- round(cooks.distance(modelo), 4)

## Valores de la diagonal de la matriz H
hii.value <- round(hatvalues(modelo), 4)

## Dffits
Dffits <- round(dffits(modelo), 4)

## Tabla de diagnósticos
resumen <- data.frame(y = datos$Cantidad_O2, y_gorro, 
                      se.y_gorro, ei, res.stud,
                      Cooks.D, hii.value, Dffits)
head(resumen)


## Tabla de valores para estimación/predicción futura

## Puntos de estimación/predicción futura
x01 <- c(1, 48, 77, 11, 54, 169, 173)
x02 <- c(1, 38, 50,  9, 40, 140, 155)

## matriz diseño
X <- model.matrix(modelo)
head(X)

x0 <- rbind(x01, x02)
colnames(x0) <- colnames(X)

## Valores h00
h00.value <- diag(x0%*%solve(t(X)%*%X)%*%t(x0))

## Valores y0_hat y se.y0_hat
tmp <- predict(modelo, newdata = data.frame(x0[, -1]), se.fit = T)

y0_hat <- tmp$fit
se.y0_hat <- tmp$se.fit

# Tabla para estimacion/prediccion futura
resumen2 <- data.frame(h00.value, y0_hat, se.y0_hat)
resumen2





