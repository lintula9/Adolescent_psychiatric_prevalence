# For combined plotting.

if( !exists( "modelList" )) source( "Scripts/SplineCutoff.R" )
if( !exists( "cohenPlot" )) source( "Scripts/CohenD.R" )
if( !exists("superiority_plots")) source( "Scripts/Superiorities.R" )

pdf( "Figures/CombinedPlots.pdf", fonts = "serif", compress = F, pointsize = 8 )

superiority_plots( PQBcutoffs, df$BDIsum )
cohenPlotCombined()
cohenPlot( )
SplinePlot( )

dev.off()
