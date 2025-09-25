# ---------------------------------------
#            PRIMER PUNTO
# ---------------------------------------
# Determinar afijación y MAE
library(tidyverse)
datos <- read.csv(file.choose())
datos <- datos[, -1]
# ---------------------------------------
# INFORMACIÓN PARA MUESTREO ESTRATIFICADO
I_ESTRATO <- datos |> 
  dplyr::group_by(ING) |> 
  dplyr::reframe(I_HOGAR = mean(I_HOGAR), Total = n()) |> 
  dplyr::mutate(Relativa = Total/sum(Total))
# ---------------------------------------
# FILTRAR POR ESTRATO
ESTRATO1 <- datos[datos$ING == 1, ]
ESTRATO2 <- datos[datos$ING == 2, ]
ESTRATO3 <- datos[datos$ING == 3, ]
# ---------------------------------------
MAS <- function(n, marco) {
  pos <- sample(nrow(marco), n, replace = F)
  muestra <- marco[pos, ]
  return(muestra)
} # Función muestreo
# ---------------------------------------
N_1 <- nrow(ESTRATO1); N_2 <- nrow(ESTRATO2); N_3 <- nrow(ESTRATO3)
N_i <- c(N_1, N_2, N_3) # Para realizar procedimiento vectorizados
# Definir el tamaño de la muestra global
n <- 8000
# Realizaar la división de las submuestras para cada estrato
# considerando el factor de afijación proporcional
psi_i <- N_i/N #  Factor de afijación
n_i <- n * psi_i
# Redonadear hacia arriba el tamaño de muestra con más observaciones
n_1 <- 3836; n_2 <- 3823; n_3 <- 342
n_i <- c(n_1, n_2, n_3)
# ---------------------------------------
# Factor de afijación de Neyman
sigma_est1 <- sd(ESTRATO1$I_HOGAR)
sigma_est2 <- sd(ESTRATO2$I_HOGAR)
sigma_est3 <- sd(ESTRATO3$I_HOGAR)
sigma_i <- c(sigma_est1, sigma_est2, sigma_est3)
# ---------------------------------------
n_iNeyman <- (N_i * sigma_i)/(sum(N_i * sigma_i)) * n

# Proceso de muestreo, con la función MAS y empleando 
# una afijación proporcional
# ---------------------------------------
ESTRATO1_muestra <- MAS(n_1, ESTRATO1)
ESTRATO2_muestra <- MAS(n_2, ESTRATO2)
ESTRATO3_muestra <- MAS(n_3, ESTRATO3)

# ---------------------------------------
#            SEGUNDO PUNTO
# ---------------------------------------
source(file.choose())
# Determinar los datos muestrales
datos_total <- list(estrato1 = ESTRATO1_muestra$I_HOGAR,
                    estrato2 = ESTRATO2_muestra$I_HOGAR,
                    estrato3 = ESTRATO3_muestra$I_HOGAR)

# Determinar la información poblacional
tamano_total <- c(estrato1 = N_1, estrato2 = N_2, estrato3 = N_3)
# Se puede usar la función estimar_parametro():
obj_total <- estimar_parametro(datos_muestrales = datos_total,
                               info_poblacion = tamano_total,
                               parametro = "tau",
                               intervalo = TRUE,
                               confianza = 0.95)
# ---------------------------------------
# Estimación del ingreso promedio
datos_promedio <- list(estrato1 = ESTRATO1_muestra$I_HOGAR,
                       estrato2 = ESTRATO2_muestra$I_HOGAR,
                       estrato3 = ESTRATO3_muestra$I_HOGAR)
# Se puede usar la función estimar_parametro():
obj_promedio <- estimar_parametro(datos_muestrales = datos_promedio,
                                  info_poblacion = tamano_total,
                                  parametro = "mu",
                                  intervalo = TRUE,
                                  confianza = 0.95)

# ---------------------------------------
#            TERCER PUNTO
# ---------------------------------------
# Los hogares que poseen automóvil
datos_CAR <- list(estrato1 = ESTRATO1_muestra$CAR,
                  estrato2 = ESTRATO2_muestra$CAR,
                  estrato3 = ESTRATO3_muestra$CAR)
# Guarda todos los tamaños poblacionales por estrato
info_poblacion <- c(estrato1 = N_1,
                    estrato2 = N_2,
                    estrato3 = N_3)
objeto_A <- estimar_parametro(datos_muestrales = datos_CAR, 
                              info_poblacion =  info_poblacion,
                              parametro = "A",
                              intervalo = TRUE,
                              confianza = 0.95)
# Los hogares que se perciben como seguros
datos_SEC <- list(estrato1 = ESTRATO1_muestra$SEC,
                  estrato2 = ESTRATO2_muestra$SEC,
                  estrato3 = ESTRATO3_muestra$SEC)
objeto_p <- estimar_parametro(datos_muestrales = datos_SEC, 
                              info_poblacion =  info_poblacion,
                              parametro = "p",
                              intervalo = TRUE,
                              confianza = 0.95)























