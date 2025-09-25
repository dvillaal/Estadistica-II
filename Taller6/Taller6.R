# ---------------------------------------
#            PRIMER PUNTO
# ---------------------------------------
# Ajuste del modelo de regresión
datos <- read.csv(file.choose())
datos$Personality <- ifelse(datos$Personality == "Introvert", 1, 0)
modelo <- glm(Personality ~ ., data = datos, family = binomial(link = "logit"))

# ---------------------------------------
#            SEGUNDO PUNTO
# ---------------------------------------
# Modelo reducido
modelo_reducido <- glm(Personality ~ 1, data = datos, family = binomial(link = "logit"))
# Considerar prueba con anova
anova(modelo_reducido, modelo, test = "Chisq")

# ---------------------------------------
#            TERCER PUNTO
# ---------------------------------------
summary(modelo)
confint(modelo)
exp(0.32057)

# ---------------------------------------
#            CUARTO PUNTO
# ---------------------------------------
x_new <- data.frame(Time_spent_Alone = 3, Social_event_attendance = 8,
                     Going_outside = 7, Friends_circle_size = 4, 
                     Post_frequency = 8, Stage_fear = "Yes")
predict.glm(modelo, newdata = x_new, type = c("response"))
