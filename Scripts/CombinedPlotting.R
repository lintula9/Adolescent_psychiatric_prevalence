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
     filename = "Figures/Publication_1_superiority.tiff", 
     res = 1080, pointsize = 12, family = "serif")
superiority_plots( sexstrat = F, ciplot = F )
dev.off()

tiff(units = "in", # 2: Cohen's D
     width = 12, height = 8, 
     filename = "Figures/Publication_2_Cohen.tiff", 
     res = 1080, pointsize = 12, family = "serif")
cohenPlotCombined( )
dev.off()

tiff(units = "in", # 3: Splines
     width = 12, height = 8, 
     filename = "Figures/Publication_3_Splines.tiff", 
     res = 1080, pointsize = 12, family = "serif")
SplinePlot( )
dev.off()

