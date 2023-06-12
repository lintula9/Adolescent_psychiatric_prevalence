# For combined plotting.
if( !exists( "df" )) source("Scripts/data_and_packages.R")
if( !exists( "modelList" )) source( "Scripts/SplineCutoff.R" )
if( !exists( "cohenPlot" )) source( "Scripts/CohenD.R" )
if( !exists("superiority_plots")) source( "Scripts/Superiorities.R" )

pdf( "Figures/CombinedPlots.pdf", fonts = "serif", compress = F, pointsize = 8 )

superiority_plots( )
cohenPlotCombined()
cohenPlot( )
SplinePlot( )

dev.off()


# Publication plots: -----

tiff(units = "in", # 1: Superiority
     width = 12, height = 8, 
     filename = "Figures/Publication_superiority_1_continuous.tiff", 
     res = 640, pointsize = 12, family = "serif")
superiority_plots( sexstrat = F, ciplot = F )
dev.off( )

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

tiff(units = "in", # 3: Cohen's D
     width = 12, height = 8, 
     filename = "Figures/Publication_Cohen.tiff", 
     res = 640, pointsize = 12, family = "serif")
cohenPlotCombined( )
dev.off( )

tiff(units = "in", # 4: Splines
     width = 12, height = 8, 
     filename = "Figures/Publication_Splines.tiff", 
     res = 640, pointsize = 12, family = "serif")
SplinePlot( )
dev.off( )


# Supplementary:

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


