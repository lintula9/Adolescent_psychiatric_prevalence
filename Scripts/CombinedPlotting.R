par(cex.main = 2,cex.main = 2)
# For combined plotting.
if( !exists( "package_names" )) source("Scripts/data_and_packages.R")
if( !exists( "modelList" )) source( "Scripts/SplineCutoff.R" )
if( !exists("superiority_plots")) source( "Scripts/Superiorities.R" )
if( !exists("cohenPlot")) source( "Scripts/CohenD.R" )

pdf( "Figures/CombinedPlots.pdf", fonts = "serif", compress = F, pointsize = 8 )

superiority_plots( )

SplinePlot( )

dev.off()


# Publication plots: -----

tiff(units = "in", # 1 Figure 1, COMBINED plot.
     width = 16, height = 9, 
     filename = "Figures/Publication_Figure1Combined.tiff", 
     res = 720, pointsize = 10, family = "serif")
par(cex.main = 2,mfrow = c(1,2), mar = c(5.1,5.1,5.1,2.1))
superiority_plots( sexstrat = F, ciplot = F )
title(main = "A", cex.main = 2)
cohenPlotCombined()
title(main = "B", cex.main = 2)
par(cex.main = 2,mar = c(5.1,4.1,4.1,2.1)) # Reset marginal settings.

# Add bootstrapped confidence intervals to plot A.


dev.off()

# # #


tiff(units = "in", # 1 Figure 3, SPLINE plot.
     width = 12, height = 9, 
     filename = "Figures/Publication_Figure2Splines.tiff", 
     res = 720, pointsize = 10, family = "serif")
par(cex.main = 2,mfrow=c(1,1),
    mar = c(5.1,5.1,4.1,2.1))
SplinePlot()
par(cex.main = 2,mfrow=c(1,1),
    mar = c(5.1,4.1,4.1,2.1))
dev.off( )

# Supplementary:
{
mars <- par()$mar

tiff(units = "in", # 2: Superiority for Mild depression
     width = 16, height = 9, 
     filename = "Figures/Publication_Superiority_2_MildAndMDD.tiff", 
     res = 640, pointsize = 12, family = "serif")
par(cex.main = 2,mfrow = c(1,2), mar = c(5.1,5.1,5.1,2.1))
superiority_plots_mildDepression( )
title(main = "A")
superiority_plots_MDD( )
title(main = "B")
dev.off( )
par(cex.main = 2,mar = mars, mfrow = c(1,1))
}
tiff(units = "in", # S1: Superiority CI plot
     width = 8, height = 12, 
     filename = "Figures/Publication_superiority_S1_SupplementaryConfInt.tiff", 
     res = 640, pointsize = 12, family = "serif")
par(cex.main = 2,mfrow = c(3,1), mar = c(5.1,5.1,5.1,2.1))

superiority_plots(plots = F, sexstrat = T, ciplot = T )
par(cex.main = 2,mfrow = c(1,1), mar = c(5.1,4.1,4.1,2.1))
dev.off( )

tiff(units = "in", # S2: Spline log likelihood plot
     width = 8, height = 12, 
     filename = "Figures/Publication_Splines_S2_loglikelihoods.tiff", 
     res = 640, pointsize = 12, family = "serif")
par(cex.main = 2,mfrow = c(3,1))
SplineLLplots( )
par(cex.main = 2,mfrow = c(1,1))
dev.off( )
