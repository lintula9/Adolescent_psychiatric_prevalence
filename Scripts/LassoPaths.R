# Lasso paths
lassoRegDat <- na.omit(df[,c("BDIsum", PQBvars)]) # Data for lassoregs.
lassoRes <- glmnet(y = lassoRegDat$BDIsum, #Lassores.
                   as.matrix(x = lassoRegDat[,PQBvars])) 
linesDf <- reshape2::melt(as.matrix(lassoRes$beta)) #figData
linesDf$l1_norm <- rep(colSums(abs(lassoRes$beta)), each = 21) #figData
linesDf$posCoef <- factor(as.numeric(rowMeans(lassoRes$beta) > 0))

pdf("Figures/LassoPaths.pdf", 
    fonts = "serif", 
    compress = F, 
    pointsize = 12, height = 12, width = 10) #PDF

cowplot::plot_grid( #Gridstart.
  ggplot(tibble( dev.ratio = lassoRes$dev.ratio, #1.Fig
                 lamda = log(lassoRes$lambda),
                 l1_norm = colSums(abs(lassoRes$beta)),
                 df = lassoRes$df,
                 dev.increment = c(NA,lassoRes$dev.ratio[2:length(lassoRes$dev.ratio)]-lassoRes$dev.ratio[1:length(lassoRes$dev.ratio)-1])
  ), aes( y = df , x = l1_norm )) +
    geom_line( color = "#7570B3" ) + 
    geom_line( aes( y = dev.ratio*41 ), lty = "dashed", color = "#D95F02" ) + 
    labs(y = "PQ-B predictor variable count", x = "Model complexity (L1 norm)") +
    scale_y_continuous(sec.axis = sec_axis( ~./41, "Proportion of BDI variance explained"), 
                       breaks = c(20, 15, 10, 5, 0)) + 
    theme(text=element_text(size=14,  family="serif")) , 
  ggplot(tibble( dev.ratio = lassoRes$dev.ratio, #2.Fig
                 lamda = log(lassoRes$lambda),
                 l1_norm = colSums(abs(lassoRes$beta)),
                 df = lassoRes$df), 
         aes( y = dev.ratio , x = l1_norm )) +
    geom_line(data = linesDf,
              aes( y = value, lty = Var1, color = posCoef) , show.legend = F) +
    labs(y = "Regression coefficient value", x = "Model complexity (L1 norm)") + 
    coord_cartesian(ylim = c(-1.5,1.5)) +
    scale_y_continuous(sec.axis = sec_axis( ~.*1, "")) +
    scale_color_manual(values = c("#7570B3", "#D95F02")) + 
    geom_abline(intercept = 0, color = "black", slope = 0) + 
    theme(text=element_text(size=14,  family="serif")), 
  align = c("v","h"), nrow = 2 #Cowplot settings.
) #GridEnd.

dev.off(); #PDF end



