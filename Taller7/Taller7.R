# ---------------------------------------
#            PRIMER PUNTO
# ---------------------------------------
library(pROC)
library(PRROC)
datos <- read.csv(file.choose())

# Ajustar el modelo
datos$Personality <- ifelse(datos$Personality == "Introvert", 1, 0)

# Se debe hacr una divisón del conjunto de datos
# Conjunto de entrenamiento y conjunto de prueba
# Con el conjunto de prueba: Validación; ROC, AUC, TCC, matriz de confusión
data_train <- datos[1:1766, ]
data_test <- datos[1767:2523, ]

# El modelo se debe ajustar con el conjunto de entrenamiento
modelo <- glm(Personality ~ ., data = data_train, family = binomial(link = "logit"))
summary(modelo)

# Considerar una variable de predicción
predict_test <- predict(modelo, newdata = data_test, type= c("response"))
roc_object <- roc.curve(scores.class0 = predict_test,
                        weights.class0 = data_test$Personality,
                        curve = TRUE)
plot(roc_object)
  
# ---------------------------------------
#            SEGUNDO PUNTO
# ---------------------------------------
y <- data_test$Personality
roc_obj <- roc(y, predict_test)
coords(roc_obj, "best", best.method = "closest.topleft")$threshold

# ---------------------------------------
#            TERCER PUNTO
# ---------------------------------------
matriz1 <- table(data_test$Personality, predict_test >= 0.8534028)
TCC1 <- sum(diag(matriz1)/sum(matriz1))

matriz2 <- table(data_test$Personality, predict_test >= 0.5)
TCC2 <- sum(diag(matriz2)/sum(matriz2))

