
# Setup of a Correlation Lower Panel in Scatterplot Matrix
myPanel.cor <- function(x, y, digits = 2, prefix = "", cex.cor){
  usr <- par("usr"); on.exit(par(usr = usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if(missing(cex.cor))
    cex = 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = 1 + 1.5*abs(r))
}

