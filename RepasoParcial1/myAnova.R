
# Tabla Anova
myAnova <- function(lm.model){
  SSq <- unlist(anova(lm.model)["Sum Sq"])
  k <- length(SSq) - 1
  SSR <- sum(SSq[1:k])
  SSE <- SSq[(k + 1)]
  MSR <- SSR/k
  df.error <- unlist(anova(lm.model)["Df"])[k + 1]
  MSE <- SSE/df.error
  F0 <- MSR/MSE
  PV <- pf(F0, k, df.error, lower.tail = F)
  result<-data.frame(Sum_of_Squares = format(c(SSR, SSE), digits = 6), DF = format(c(k, df.error), digits = 6),
                     Mean_Square = format(c(MSR, MSE), digits = 6), F_Value = c(format(F0, digits = 6), ''),
                     P_value = c(format(PV, digits = 6), ''), row.names = c("Model", "Error"))
  result
}