
# Setup of a Correlation Lower Panel in Scatterplot Matrix
myPanel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  # Para definir región de graficiación
  par(usr = c(usr[1:2], 0, 1.5) )
  # Para obtener una lista que guarde las marcas de clase y conteos en cada una:
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks;
  nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  # Para dibujar los histogramas
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

