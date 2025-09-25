# ---------------------------------------
#            PRIMER PUNTO
# ---------------------------------------
datos <- read.csv(file.choose())
modelo <- glm(Publicaciones ~ ., data = datos, family = poisson(link = "log"))

summary(modelo)

# ---------------------------------------
#            SEGUNDO PUNTO
# ---------------------------------------
modelo_reducido <- glm(Publicaciones ~ 1, data = datos, family = poisson(link = "log"))
anova(modelo_reducido, modelo, test = "Chisq")

# ---------------------------------------
#            TERCER PUNTO
# ---------------------------------------
summary(modelo)

# ---------------------------------------
#            CUARTO PUNTO
# ---------------------------------------
lambda <- exp(1.207 + 0.07 * 5 + 0.05 * 5 - 0.33 * 1)





