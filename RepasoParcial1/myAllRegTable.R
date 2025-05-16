
# All Posible Regressions Table
myAllRegTable <- function(lm.model, response = model.response(model.frame(lm.model)), MSE = F){
  regTable <- summary(leaps::regsubsets(model.matrix(lm.model)[, -1], response,
                                 nbest = 2^(lm.model$rank - 1) - 1, really.big = T))
  pvCount <- as.vector(apply(regTable$which[, -1], 1, sum))
  pvIDs <- apply(regTable$which[, -1], 1, function(x) as.character(paste(colnames(model.matrix(lm.model)[, -1])[x],
                                                                         collapse = " ")))
  result <- if(MSE){
    data.frame(k = pvCount, R_sq = round(regTable$rsq, 3), adj_R_sq = round(regTable$adjr2, 3),
               MSE = round(regTable$rss/(nrow(model.matrix(lm.model)[,-1]) - (pvCount + 1)), 3),
               Cp = round(regTable$cp, 3), Variables_in_model = pvIDs)
  } else {
    data.frame(k = pvCount, R_sq = round(regTable$rsq, 3), adj_R_sq = round(regTable$adjr2, 3),
               SSE = round(regTable$rss, 3),
               Cp = round(regTable$cp, 3), Variables_in_model = pvIDs)
  }
  format(result, digits = 6)
}