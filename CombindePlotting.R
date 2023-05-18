# For combined plotting.


pdf("CombinedPlots.pdf", fonts = "serif", compress = F, pointsize = 8)


SplinePlot()
superiority_plots(PQBcutoffs,df$BDIMDD) ;  title(main = "Superiority plot for major depression")
superiority_plots(PQBcutoffs,df$BDImild) ;  title(main = "Superiority plot for mild depression")
linRegPlot
LassoPlot()
dev.off()