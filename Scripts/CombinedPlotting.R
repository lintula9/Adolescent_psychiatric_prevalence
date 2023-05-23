# For combined plotting.

source("CohenD.R")
source("Superiorities.R")

pdf("Figures/CombinedPlots.pdf", fonts = "serif", compress = F, pointsize = 8)

superiority_plots(PQBcutoffs,df$BDIsum)
cohenPlot()

dev.off()
