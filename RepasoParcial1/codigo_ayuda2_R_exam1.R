
library(rio)
library(here)
datos <- rio::import(here(file.choose()))

### ver la estructura de los datos
head(datos)

## Matriz de gráficas de dispersión con boxplots 
## y correlaciones

source(file.choose())

pairs(datos, lower.panel = myPanel.cor, upper.panel = panel.smooth,
      diag.panel = myPanel.box, 
      labels = names(datos))

## Matriz de Correlación
cor(datos)

## Ajuste del Modelo
modelo <- lm(Y ~ X1 + X2 + X3 + X4, data = datos)

summary(modelo)
summary(modelo)$coefficients

source(file.choose())
myAnova(modelo)

## Uso de la función de usuario myCoefficients()

## Cálculo de coeficientes de regresión, 
## crudos, estandarizados y VIF
source(file.choose())
myCoefficients(modelo, datos)

## Análisis de los valores propios de la matriz X′X

library(olsrr)
ols_coll_diag(modelo)

#ols_eigen_cindex(modelo)
#ols_vif_tol(modelo)


source(file.choose())
myAllRegTable(modelo)

### MR PH-SUB_CONJUNTO

modelo_MR <- lm(Y ~ X1 + X2 + X3, data = datos)

summary(modelo_MR)
summary(modelo_MR)$coefficients

source(file.choose())
myAnova(modelo_MR)

### MR PHLG
## Datos para MR en PHLG
datos <- modelo$model         ## matriz de datos-xY
head(datos)

## Matriz Diseño X
X <- model.matrix(modelo)
head(X)

datos_MR_PHLG <- data.frame(datos$Y,datos$X1+datos$X2,
                     datos$X3+datos$X4)

colnames(datos_MR_PHLG) <- c("Y","X12","X34")
head(datos_MR_PHLG)

## Ajuste del MR en PHLG

modelo_mr_phlg <- lm(Y ~., data=datos_MR_PHLG)
source(file.choose())
myAnova(modelo_mr_phlg)


summary(modelo_mr_phlg)
summary(modelo_mr_phlg)$coefficients


## Validación de supuestos de errores

## Prueba de normalidad de errores
source(file.choose())
myQQnorm(modelo)

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


## Tabla de resumen para diagnóstico de valores extremos

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
resumen <- data.frame(y = datos$Y, y_gorro, 
                      se.y_gorro, ei, res.stud,
                      Cooks.D, hii.value, Dffits)
head(resumen)


## Tabla de valores para estimación/predicción futura

## Puntos de estimación/predicción futura
x01 <- c(1, 48, 77, 11, 54)
x02 <- c(1, 38, 50,  9, 40)

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






