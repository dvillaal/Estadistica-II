
# Setup of a Boxplot Diagonal Panel in Scatterplot Matrix
myPanel.box <- function(x, ...){
  usr <- par("usr", bty = 'n')
  on.exit(par(usr))
  par(usr = c(-1, 1, min(x) - 0.5, max(x) + 0.5))
  b <- boxplot(x, plot = F)
  whisker.i <- b$stats[1,]
  whisker.s <- b$stats[5,]
  hinge.i <- b$stats[2,]
  mediana <- b$stats[3,]
  hinge.s <- b$stats[4,]
  rect(-0.5, hinge.i, 0.5, mediana, col = 'gray')
  segments(0, hinge.i, 0, whisker.i, lty = 2)
  segments(-0.1, whisker.i, 0.1, whisker.i)
  rect(-0.5, mediana, 0.5, hinge.s, col = 'gray')
  segments(0, hinge.s, 0, whisker.s, lty = 2)
  segments(-0.1, whisker.s, 0.1, whisker.s)
}
