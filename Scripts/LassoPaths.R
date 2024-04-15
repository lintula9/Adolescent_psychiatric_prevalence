# Lasso paths
lassoRegDat <- na.omit(df[,c("BDIsum", PQBvars, "sex", "Age_in_years")]) # Data for lassoregs.
lassoRegDat$sex <- as.numeric(lassoRegDat$sex) - 1
nVars <- ncol(lassoRegDat)

lassoRes <- glmnet(y = lassoRegDat$BDIsum, #Lassores.
                   as.matrix(x = lassoRegDat[,c(PQBvars, "sex", "Age_in_years")]), ) 
lassoRes$l1_norm <- colSums(abs(lassoRes$beta))


linesDf <- reshape2::melt(as.matrix(lassoRes$beta)) #figData
linesDf$l1_norm <- rep( colSums(abs(lassoRes$beta)), each = nVars - 1 ) #fig Data
linesDf$Var1 <- gsub(pattern = "PQB",replacement = "PQ-B ",x = linesDf$Var1)
lastVals <- linesDf[(nrow(linesDf) - nVars + 2):nrow(linesDf) , ]
lastVals$Rank <- order(lastVals$value)
lastVals <- lastVals[ order(lastVals$Rank), ]
linesDf <- merge(linesDf, lastVals[,c("Var1","Rank")], by = "Var1", all.y = T)
linesDf <- linesDf[ order(linesDf$Rank), ]

pqbcols <- c()
pqbcols[ lastVals$Var1 ] <- c(rep(cols, times = 7), NA, NA)
names(pqbcols) <- lastVals$Var1
pqbltys <- c()
pqbltys[ lastVals$Var1 ] <- c(rep(c(1,2,3), times = 7), NA, NA)
names(pqbltys) <- lastVals$Var1

set.seed(1)
BestLamda <- cv.glmnet(y = lassoRegDat$BDIsum, #Lassores.
                       x = as.matrix(x = lassoRegDat[,PQBvars]), 
                       type.measure = "mse", nfolds = nrow(lassoRegDat))
bestLamdaIndex <- BestLamda$index[ 1 ]
approximations <-approxfun(x = lassoRes$l1_norm,y = lassoRes$df, method = "constant", f = 1)

if(FALSE){

tiff("Figures/Figure1_Combined.tiff",
     pointsize = 12,
     height = 8,
     width = 14, res = 480,
     units = "in", family = "serif")

  # tiff("Figures/Figure1_VarianceExplained.tiff",
  #   pointsize = 12,
  #   height = 8,
  #   width = 8, res = 480,
  #   units = "in", family = "serif")
cowplot::plot_grid(align = "hv", labels = "AUTO", 
                   label_fontfamily = "serif", 
  #Gridstart.
  # ggplot(tibble( dev.ratio = lassoRes$dev.ratio, #1.Fig
  #                lamda = log(lassoRes$lambda),
  #                l1_norm = colSums(abs(lassoRes$beta)),
  #                df = lassoRes$df,
  #                dev.increment = lassoRes$dev.ratio / colSums(abs(lassoRes$beta))), 
  #        aes( y = df , x = l1_norm )) +
  #   geom_line( color = "#7570B3" ) + 
  #   labs(y = expression(df), x = expression("||"* bold("\u03B2") *"||")) +
  #   theme(text = element_text( size=14,  family="serif")) +
  #   ggtitle("A"), 
  
  ggplot(tibble( dev.ratio = lassoRes$dev.ratio, #2.Fig
                 lamda = log(lassoRes$lambda),
                 l1_norm = colSums(abs(lassoRes$beta)),
                 df = lassoRes$df,
                 dev.increment = lassoRes$dev.ratio / colSums(abs(lassoRes$beta))),
         aes( y = dev.ratio , x = l1_norm )) +
    scale_x_continuous(sec.axis = sec_axis( ~.*1, ""), minor_breaks = NULL) +
    scale_y_continuous(sec.axis = sec_axis( ~.*nVars, "Variables included", 
                                            breaks = seq(1,nVars,2))) + 
    geom_line( aes(color = "Var. explained") , col = cols[ 2 ] ) +
    geom_line(data = tibble(l1_norm = seq(0,max(lassoRes$l1_norm),.005),
                            df = approximations( seq( 0, max( lassoRes$l1_norm ), .005 ) ) ), 
              inherit.aes = F, 
              aes( x = l1_norm, color = "df", y = df / nVars ), col = cols[ 3 ], lty = 2 ) +
    labs( y = "Variance explained", x = expression( "L1 norm" ) ) +
    theme(text=element_text(size=14,  family="serif")) +
    geom_vline( xintercept = lassoRes$l1_norm[ bestLamdaIndex ],
                col = "darkgray"), 

  # dev.off()
  # ggplot(tibble( dev.ratio = lassoRes$dev.ratio, #3.Fig
  #                lamda = log(lassoRes$lambda),
  #                l1_norm = colSums(abs(lassoRes$beta)),
  #                df = lassoRes$df,
  #                dev.increment = lassoRes$dev.ratio / colSums(abs(lassoRes$beta))),
  #        aes( y = dev.increment , x = l1_norm )) +
  #   geom_line( color = "#D95F02" ) + 
  #   labs(y = expression(frac(R^2,  "||"* bold("\u03B2") *"||")), x = expression("||"* bold("\u03B2") *"||"), ) +
  #   theme(text=element_text(size=14,  family="serif"))+
  #   ggtitle("C"), 
  
# tiff("Figures/Figure2_LassoPaths.tiff", 
#        pointsize = 12, 
#        height = 8, width = 8, res = 480, 
#      units = "in", family = "serif") 

  ggplot( linesDf[ !(linesDf$Var1 %in% c("sex", "Age_in_years")), ], # sex is omitted from this plot.
          aes( y = value, color = Var1, x = l1_norm )) +
    geom_line( ) +
    labs(y = "Coefficient", x = "L1 norm") + 
    coord_cartesian(ylim = c(-1.5,1.5), xlim = c(0, 20)) +
    scale_y_continuous(sec.axis = sec_axis( ~.*1, "")) +
    scale_x_continuous(sec.axis = sec_axis( ~.*1, "", breaks = seq(0,18,4)), breaks = seq(0,18,4), minor_breaks = NULL) +
    scale_color_manual( values = pqbcols ) +
    scale_linetype_manual( values = pqbltys ) +
    annotate("segment", x = 0, y = 0, xend = max(linesDf$l1_norm), yend = 0) + 
    theme(text=element_text(size=14,  family="serif")) +
    geom_text_repel(data = lastVals[ !(lastVals$Var1 %in% c("sex", "Age_in_years")), ], # Sex is omitted from this plot. 
                     aes(label = Var1), 
                     nudge_x = 1, color = pqbcols[-which(names(pqbcols) %in% c("sex", "Age_in_years"))],
                     segment.linetype = 3, direction = "y", hjust=0) + 
    geom_vline( xintercept = lassoRes$l1_norm[ bestLamdaIndex ],  
                col = "darkgray") + 
    guides(label = "none", color = "none")
# dev.off()
 
)#GridEnd.

dev.off(); #TIFF end



}
if(FALSE){
tiff("Figures/SupplementaryFigure_MSEplot.tiff",
     pointsize = 12,
     height = 8,
     width = 14, res = 480,
     units = "in", family = "serif")
plot(BestLamda)
dev.off()
  
  
  
  
}