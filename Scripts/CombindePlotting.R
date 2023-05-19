# For combined plotting.


pdf("CombinedPlots.pdf", fonts = "serif", compress = F, pointsize = 10)


SplinePlot()
SplineAICplots()
linRegPlot
superiority_plots(PQBcutoffs,df$BDIMDD) ;  title(main = "Superiority plot for major depression")
superiority_plots(PQBcutoffs,df$BDImild) ;  title(main = "Superiority plot for mild depression")

dev.off()
