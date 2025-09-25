# ------------------------------
#         Código guía
# ------------------------------
library(tidyverse)
# Determinar los datos poblacionales
datos_poblacion <- read.csv(file.choose())
datos_poblacion <- datos_poblacion[, -1]
# Determinar los datos muestrales
datos_muestra <- read.csv(file.choose())
datos_muestra <- datos_muestra[, -1]
# ------------------------------
#       Preparación datos
# ------------------------------
# -------------------------
#    Librerías necesarias
# -------------------------
source(file.choose()) # Cargar función de estimación
# -------------------------
xprint <- function(x){
  sprintf(x, fmt = "%#.4f")
} # Termina la función
# -------------------------
# Realizar la division de los datos por estratos
# PARA LA POBLACIÓN
ESTRATO1_poblacion = datos_poblacion[datos_poblacion$ING == 1, ]
ESTRATO2_poblacion = datos_poblacion[datos_poblacion$ING == 2, ]
ESTRATO3_poblacion = datos_poblacion[datos_poblacion$ING == 3, ]
# PARA LA MUESTRA
ESTRATO1_muestra = datos_muestra[datos_muestra$ING == 1, ]
ESTRATO2_muestra = datos_muestra[datos_muestra$ING == 2, ]
ESTRATO3_muestra = datos_muestra[datos_muestra$ING == 3, ]

# ------------------------------
#         Primer punto
# ------------------------------
# INFORMACIÓN PARA MUESTREO ESTRATIFICADO
I_ESTRATO <- datos_poblacion |> 
  dplyr::group_by(ING) |> 
  dplyr::reframe(I_HOGAR = mean(I_HOGAR), Total = n(), Proporcion_CAR = mean(CAR)) |> 
  dplyr::mutate(Relativa = Total/sum(Total))

carro_poblacion <- datos_poblacion |> 
  dplyr::group_by(ING) |> 
  dplyr::reframe(Total = n()) |> 
  dplyr::mutate(proporcion = Total/nrow(datos_poblacion))

N_1 <- nrow(ESTRATO1_poblacion) # 43597 en este caso
n_1 <- nrow(ESTRATO1_muestra) # 5086 en este caso

xprint(I_ESTRATO$I_HOGAR)
xprint(I_ESTRATO$Proporcion_CAR)
xprint(I_ESTRATO$Relativa)

# ------------------------------
# Ingreso por hogar
# MU
ingreso_estrato1 <- ESTRATO1_muestra$I_HOGAR
objeto_mu <- estimar_parametro(datos_muestrales = ingreso_estrato1, 
                               info_poblacion = N_1, 
                               parametro = "mu", 
                               intervalo = TRUE,
                               confianza = 0.90)
xprint(mu_gorro <- objeto_mu$Estimador)
xprint(mu_gorro <- objeto_mu$Varianza_Estimador)
xprint(mu_gorro <- objeto_mu$IC)
# ------------------------------
# TAU
objeto_tau <- estimar_parametro(datos_muestrales = ingreso_estrato1, 
                                info_poblacion = N_1, 
                                parametro = "tau", 
                                intervalo = TRUE,
                                confianza = 0.90)
xprint(tau_gorro <- objeto_tau$Estimador)
xprint(tau_gorro <- objeto_tau$Varianza_Estimador)
xprint(tau_gorro <- objeto_tau$IC)
# ------------------------------
# Posesión de automóvil
# p
automovil_estrato1 <- ESTRATO1_muestra$CAR
objeto_p <- estimar_parametro(datos_muestrales = automovil_estrato1, 
                              info_poblacion = N_1, 
                              parametro = "p", 
                              intervalo = TRUE,
                              confianza = 0.90)
xprint(p_gorro <- objeto_p$Estimador)
xprint(p_gorro <- objeto_p$Varianza_Estimador)
xprint(p_gorro <- objeto_p$IC)
# ------------------------------
# A
objeto_A <- estimar_parametro(datos_muestrales = automovil_estrato1, 
                              info_poblacion = N_1, 
                              parametro = "A", 
                              intervalo = TRUE,
                              confianza = 0.90)
xprint(A_gorro <- objeto_A$Estimador)
xprint(A_gorro <- objeto_A$Varianza_Estimador)
xprint(A_gorro <- objeto_A$IC)

# ------------------------------
#         Segundo punto
# ------------------------------
# Definición de tamaños poblacionales
N_1 <- nrow(ESTRATO1_poblacion) # 43597 en este caso
N_2 <- nrow(ESTRATO2_poblacion) # 38644 en este caso
N_3 <- nrow(ESTRATO3_poblacion) # 3487 en este caso

# Definición de tamaños muestrales
n_1 <- nrow(ESTRATO1_muestra) # 5086 en este caso
n_2 <- nrow(ESTRATO2_muestra) # 4508 en este caso
n_3 <- nrow(ESTRATO3_muestra) # 407 en este caso
# ------------------------------
# ESTRATO 1
ingreso_estrato1 <- ESTRATO1_muestra$I_HOGAR
objeto_mu1 <- estimar_parametro(datos_muestrales = ingreso_estrato1, 
                                info_poblacion = N_1, 
                                parametro = "mu", 
                                intervalo = TRUE,
                                confianza = 0.95)
xprint(mu_gorro1 <- objeto_mu1$Estimador)
xprint(mu_gorro1 <- objeto_mu1$Varianza_Estimador)
xprint(mu_gorro1 <- objeto_mu1$IC)
# ------------------------------
# ESTRATO 2
ingreso_estrato2 <- ESTRATO2_muestra$I_HOGAR
objeto_mu2 <- estimar_parametro(datos_muestrales = ingreso_estrato2, 
                                info_poblacion = N_2, 
                                parametro = "mu", 
                                intervalo = TRUE,
                                confianza = 0.95)
xprint(mu_gorro2 <- objeto_mu2$Estimador)
xprint(mu_gorro2 <- objeto_mu2$Varianza_Estimador)
xprint(mu_gorro2 <- objeto_mu2$IC)
# ------------------------------
# ESTRATO 3
ingreso_estrato3 <- ESTRATO3_muestra$I_HOGAR
objeto_mu3 <- estimar_parametro(datos_muestrales = ingreso_estrato3, 
                                info_poblacion = N_3,
                                parametro = "mu", 
                                intervalo = TRUE,
                                confianza = 0.95)
xprint(mu_gorro3 <- objeto_mu3$Estimador)
xprint(mu_gorro3 <- objeto_mu3$Varianza_Estimador)
xprint(mu_gorro3 <- objeto_mu3$IC)
# ------------------------------
# TODOS
datos_promedio <- list(estrato1 = ESTRATO1_muestra$I_HOGAR,
                       estrato2 = ESTRATO2_muestra$I_HOGAR,
                       estrato3 = ESTRATO3_muestra$I_HOGAR)

tamano_total <- c(estrato1 = N_1, estrato2 = N_2, estrato3 = N_3)

obj_total <- estimar_parametro(datos_muestrales = datos_promedio,
                               info_poblacion = tamano_total,
                               parametro = "mu",
                               intervalo = TRUE,
                               confianza = 0.95)
xprint(mu_gorroTotal <- obj_total$Estimador)
xprint(mu_gorroTotal <- obj_total$Varianza_Estimador)
xprint(mu_gorroTotal <- obj_total$IC)

# ------------------------------
#         Tercer punto
# ------------------------------

# ------------------------------
alpha <- 0.1; delta <- 600000; Z <- qnorm(1 - alpha/2)
D <- delta^2/Z^2
psi_i <- N_i/N # Afijación proporcional
N <- nrow(datos);
p_i <- c(p_hat1, p_hat2, p_hat3)
# ------------------------------
# Determinación del tamaño de muestra global
n <- sum((N_i^2 * p_i * (1 - p_i))/psi_i)/((N^2 * D) + sum(N_i * p_i * (1 - p_i)))
n_1 <- ceiling(psi_i[1] * n)
n_2 <- ceiling(psi_i[2] * n)
n_3 <- ceiling(psi_i[3] * n)
n_redond <- n_1 + n_2 + n_3
# ------------------------------
# p
p_hat1 <- estimar_parametro(ESTRATO1_muestra$REC, 
                            N_1, 
                            parametro = "p")$Estimador
p_hat2 <- estimar_parametro(ESTRATO2_muestra$REC, 
                            N_2, 
                            parametro = "p")$Estimador
p_hat3 <- estimar_parametro(ESTRATO3_muestra$REC, 
                            N_3, 
                            parametro = "p")$Estimador
p_i <- c(p_hat1, p_hat2, p_hat3)
# ------------------------------
alpha <- 0.1; delta <- 0.02; Z <- qnorm(1 - alpha/2)
D <- delta^2/Z^2
psi_i <- N_i/N # Afijación proporcional
N <- nrow(datos);
p_i <- c(p_hat1, p_hat2, p_hat3)
# ------------------------------
# Determinación del tamaño de muestra global
n <- sum((N_i^2 * p_i * (1 - p_i))/psi_i)/((N^2 * D) + sum(N_i * p_i * (1 - p_i)))
n_1 <- ceiling(psi_i[1] * n)
n_2 <- ceiling(psi_i[2] * n)
n_3 <- ceiling(psi_i[3] * n)
n_redond <- n_1 + n_2 + n_3




# -------------------------------------------------------
N_1 <- nrow(ESTRATO1_poblacion); N_2 <- nrow(ESTRATO2_poblacion); N_3 <- nrow(ESTRATO3_poblacion)
N_i <- c(N_1, N_2, N_3)
N <- sum(N_i)
# MU
mu_hat1 <- estimar_parametro(ESTRATO1_muestra$I_HOGAR, 
                             N_1, 
                             parametro = "mu")$Estimador
mu_hat2 <- estimar_parametro(ESTRATO2_muestra$I_HOGAR, 
                             N_2, 
                             parametro = "mu")$Estimador
mu_hat3 <- estimar_parametro(ESTRATO3_muestra$I_HOGAR, 
                             N_3, 
                             parametro = "mu")$Estimador
m_i <- c(mu_hat1, mu_hat2, mu_hat3)

alpha_muestra <- 0.1; Z <- qnorm(1- alpha_muestra/2);
delta <- 600000; D <- (delta^2)/(Z^2);
N_i <- c(N_1, N_2, N_3); psi_i <- m_i; # Afijación proporcional
# -------------------------
# También se deben tener en cuenta las varianzas poblacionales
# para cada estrato (1, 2, 3) en el ingreso
# Determinar las varianzas poblacionales por estrato
sigma2_i <- c(var(ESTRATO1_poblacion$I_HOGAR),
              var(ESTRATO2_poblacion$I_HOGAR),
              var(ESTRATO3_poblacion$I_HOGAR))
# El anterior objeto es el vector de varianzas del ingreso
# Según la fórmula para la estimación de n:
n_mu <- sum(((N_i^2) * sigma2_i)/psi_i)/(N^2 * D + sum(N_i * sigma2_i))
n_1mu <- ceiling(psi_i[1] * n_mu)
n_2mu <- ceiling(psi_i[2] * n_mu)
n_3mu <- ceiling(psi_i[3] * n_mu)
n_redondmu <- n_1mu + n_2mu + n_3mu


























