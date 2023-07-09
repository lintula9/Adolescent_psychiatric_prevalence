# Lasso paths
lassoRegDat <- na.omit(df[,c("BDIsum", PQBvars)]) # Data for lassoregs.
lassoRes <- glmnet(y = lassoRegDat$BDIsum, #Lassores.
                   as.matrix(x = lassoRegDat[,PQBvars])) 
linesDf <- reshape2::melt(as.matrix(lassoRes$beta)) #figData
linesDf$l1_norm <- rep(colSums(abs(lassoRes$beta)), each = 21) #figData
linesDf$Var1 <- gsub(pattern = "PQB",replacement = "PQ-B ",x = linesDf$Var1)
lastVals <- linesDf[(nrow(linesDf)-20):nrow(linesDf) , ]
lastVals$Rank <- order(lastVals$value)
lastVals <- lastVals[ lastVals$Rank, ]
linesDf <- merge(linesDf, lastVals[,c("Var1","Rank")], by = "Var1", all.y = T)
linesDf <- linesDf[ order(linesDf$Rank), ]
pqbcols <- c()
pqbcols[ lastVals$Var1 ] <- rep(cols, times = 7)
names(pqbcols) <- lastVals$Var1
pqbltys <- c()
pqbltys[ lastVals$Var1 ] <- rep(c(1,2,3), times = 7)
names(pqbltys) <- lastVals$Var1

if(FALSE){
tiff("Figures/Figure1_VarianceExplained.tiff", 
    pointsize = 12, 
    height = 8, 
    width = 8, res = 480, 
    units = "in", family = "serif") 

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
    geom_line( color = "#D95F02" ) +
    labs(y = "Variance explained", x = expression("L1 norm")) +
    theme(text=element_text(size=14,  family="serif")) +
    ggtitle("A")

  dev.off()
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
  
tiff("Figures/Figure2_LassoPaths.tiff", 
       pointsize = 12, 
       height = 8, width = 8, res = 480, 
     units = "in", family = "serif") 
par(adj = 0)

  ggplot( linesDf, 
          aes( y = value, color = Var1, x = l1_norm )) +
    geom_line( ) +
    labs(y = "Coefficient", x = "L1 norm") + 
    coord_cartesian(ylim = c(-1.5,1.5), xlim = c(0, 14)) +
    scale_y_continuous(sec.axis = sec_axis( ~.*1, "")) +
    scale_x_continuous(sec.axis = sec_axis( ~.*1, "", breaks = seq(0,12,4)), breaks = seq(0,12,4), minor_breaks = NULL) +
    scale_color_manual( values = pqbcols ) +
    scale_linetype_manual( values = pqbltys ) +
    annotate("segment", x = 0, y = 0, xend = max(linesDf$l1_norm), yend = 0) + 
    theme(text=element_text(size=14,  family="serif")) +
    geom_text_repel(data = lastVals, 
                     aes(label = Var1), 
                     nudge_x = 1, color = pqbcols,
                     segment.linetype = 3, direction = "y", hjust=0) + 
    ggtitle("B") + 
    guides(label = "none", color = "none")
  par(adj = 0.5)
dev.off()
 
  #GridEnd.

dev.off(); #PDF end

(apply(as.matrix(t(lassoRes$beta)), FUN = function(x) sum(x == 0), MARGIN = 2)
)

}
