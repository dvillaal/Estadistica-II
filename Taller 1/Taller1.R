%>% %>% # ----------------------------------------------------------------------------

# Y = Performance
# X1 = Stregth
# X1 = Stregth
# X2 = Skills
# X3 = Speed

# Yi = B0 + B1Xi1 + B2Xi2 + B3Xi3 + error
# Yi_gorro = B0_gorro +  B1_gorroXi1 + B2_gorroXi2 + B3_gorroXi3 + error
# Perfi_gorro = B0_gorro +  B1_gorroSti + B2_gorroSki + B3_gorroSpi + error

# ---------------------------
#       PRIMER PUNTO
# ---------------------------

datos <- read.csv(file.choose())

# Hallar matriz de diseño
Y <- datos$Performance
X1 <- datos$Strength; X2 <- datos$Skills; X3 <- datos$Speed;

modelo <- lm(Y ~ X1 + X2 + X3)
X = model.matrix(modelo)

# Hallar vector de parámetros esperados
B = solve(t(X) %*% X) %*% t(X) %*% Y

# ---------------------------
#       SEGUNDO PUNTO
# ---------------------------
Y_gorro <- X %*% B

# ---------------------------
#       TERCER PUNTO
# ---------------------------
errores_estimados <- Y - Y_gorro

# ---------------------------
#       CUARTO PUNTO
# ---------------------------
n = nrow(datos); p = length(datos)
MSE = t(errores_estimados) %*% errores_estimados/(n-p)

# ---------------------------
summary(modelo)















