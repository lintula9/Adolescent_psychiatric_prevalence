# For combined plotting.
if( !exists( "package_names" )) source("Scripts/data_and_packages.R")
if( !exists( "modelList" )) source( "Scripts/SplineCutoff.R" )
if( !exists("superiority_plots")) source( "Scripts/Superiorities.R" )

pdf( "Figures/CombinedPlots.pdf", fonts = "serif", compress = F, pointsize = 8 )

superiority_plots( )

SplinePlot( )

dev.off()


# Publication plots: -----

tiff(units = "in", # 1 Figure 1, COMBINED plot.
     width = 16, height = 9, 
     filename = "Figures/Publication_Figure1Combined.tiff", 
     res = 720, pointsize = 10, family = "serif")
par(mfrow = c(1,2))
superiority_plots( sexstrat = F, ciplot = F )
title(main = "A", cex.main = 1.5)
# # #
# DORPLOT HERE !!!!!!
tiff(units = "in", # 2 Figure 3, DORPLOT plot.
     width = 16, height = 9, 
     filename = "Figures/Publication_Figure3DOR.tiff", 
     res = 720, pointsize = 10, family = "serif")
par(mfrow = c(1,2) )
DORplot_mild()
DORplot_MDD()
par(mfrow=c(1,1))
dev.off()

# # #
title(main = "B", cex.main = 1.5)
par(mfrow=c(1,1))
dev.off( )

tiff(units = "in", # 1 Figure 3, SPLINE plot.
     width = 12, height = 9, 
     filename = "Figures/Publication_Figure2Splines.tiff", 
     res = 720, pointsize = 10, family = "serif")
par(mfrow=c(1,1),
    mar = c(5.1,5.1,4.1,2.1))
SplinePlot()
par(mfrow=c(1,1),
    mar = c(5.1,4.1,4.1,2.1))
dev.off( )

# Supplementary:

tiff(units = "in", # 2: Superiority for Mild depression
     width = 12, height = 8, 
     filename = "Figures/Publication_Superiority_2_ForMildDepression.tiff", 
     res = 640, pointsize = 12, family = "serif")
superiority_plots_mildDepression( )
dev.off( )

tiff(units = "in", # 2: Superiority for Mild depression
     width = 12, height = 8, 
     filename = "Figures/Publication_Superiority_3_ForMDD.tiff", 
     res = 640, pointsize = 12, family = "serif")
superiority_plots_MDD( )
dev.off( )

tiff(units = "in", # S1: Superiority CI plot
     width = 8, height = 12, 
     filename = "Figures/Publication_superiority_S1_SupplementaryConfInt.tiff", 
     res = 640, pointsize = 12, family = "serif")
par(mfrow = c(3,1))
superiority_plots(plots = F, sexstrat = T, ciplot = T )
par(mfrow = c(1,1))
dev.off( )

tiff(units = "in", # S2: Spline log likelihood plot
     width = 8, height = 12, 
     filename = "Figures/Publication_Splines_S2_loglikelihoods.tiff", 
     res = 640, pointsize = 12, family = "serif")
par(mfrow = c(3,1))
SplineLLplots( )
par(mfrow = c(1,1))
dev.off( )

tiff(units = "in", # S4: DOR CIs
     width = 8, height = 12, 
     filename = "Figures/Publication_DOR_S4_CIs.tiff", 
     res = 640, pointsize = 12, family = "serif")
par(mfrow = c(2,1))
DORplot_mild_CIs()
DORplot_MDD_CIs()
par(mfrow = c(1,1))
dev.off( )


