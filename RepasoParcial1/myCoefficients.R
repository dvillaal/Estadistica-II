
# Extract estimated and standardized coefficients, their 95% CI's and VIF's
myCoefficients <- function(lm.model, dataset){
  library(car)
  coeff <- coef(lm.model)
  scaled.data <- as.data.frame(scale(dataset))
  coef.std <- c(0, coef(lm(update(formula(lm.model), ~.+0), scaled.data)))
  limites <- confint(lm.model, level = 0.95)
  vifs <- c(0, vif(lm.model))
  result <- data.frame(Estimation = coeff, Coef.Std = coef.std, 
                       Limits = limites, Vif = vifs)
  names(result)[3:4] <- c("Limit_2.5%","Limit_97.5%")
  cat("Estimated and standardized coefficients, their 95% CI's and VIF's", "\n")
  result
}

