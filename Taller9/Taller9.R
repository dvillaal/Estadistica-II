# ---------------------------------------
#            PRIMER PUNTO
# ---------------------------------------
datos <- read.csv(file.choose())
datos <- datos[, -1]
# Función para muetreo aleatorio simple sin reemplazo
MAS <- function(n, marco) {
  pos <- sample(nrow(marco), n, replace = F)
  muestra <- marco[pos, ]
  return(muestra)
} # Función muestreo

# Ingresa: n (tamaño de la muestra; marco: la información muestreo)
muestra <- MAS(5000, datos)

# ---------------------------------------
#            SEGUNDO  PUNTO
# ---------------------------------------
source(file.choose())
ingreso_muestra <- muestra$I_HOGAR
N <- 81961
# tau
obj_tau <- estimar_parametro(ingreso_muestra, N, parametro = "tau", intervalo = TRUE, confianza = 0.95)
tau_estimado <- obj_tau$Estimador

# mu
obj_mu <- estimar_parametro(ingreso_muestra, N, parametro = "mu", intervalo = TRUE, confianza = 0.95)
mu_estimado <- obj_mu$Estimador

# ---------------------------------------
#            TERCER  PUNTO
# ---------------------------------------
seguros_muestra <- muestra$SEC
automoviles_muestra <- muestra$CAR
obj_p <- estimar_parametro(seguros_muestra, N, parametro = "p", intervalo = TRUE, confianza = 0.95)
obj_A <- estimar_parametro(automoviles_muestra, N, parametro = "A", intervalo = TRUE, confianza = 0.95)

# ---------------------------------------
#            CUARTO  PUNTO
# ---------------------------------------
recreativo_muestra <- muestra$REC
obj_prec <- estimar_parametro(recreativo_muestra, N, parametro = "p")
p_rec <- obj_prec$Estimador
D <- (0.02)^2/(qnorm(0.05, lower.tail = FALSE)^2)
n <- (N * p_rec * (1 - p_rec))/((N - 1) * D + p_rec * (1 - p_rec))












