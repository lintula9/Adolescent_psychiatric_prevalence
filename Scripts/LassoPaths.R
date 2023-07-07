# Lasso paths
lassoRegDat <- na.omit(df[,c("BDIsum", PQBvars)]) # Data for lassoregs.
lassoRes <- glmnet(y = lassoRegDat$BDIsum, #Lassores.
                   as.matrix(x = lassoRegDat[,PQBvars])) 
linesDf <- reshape2::melt(as.matrix(lassoRes$beta)) #figData
linesDf$l1_norm <- rep(colSums(abs(lassoRes$beta)), each = 21) #figData
linesDf$posCoef <- factor(as.numeric(rowMeans(lassoRes$beta) > 0))

tiff("Figures/LassoPaths.tiff", 
    pointsize = 12, 
    height = 9, 
    width = 6, res = 480, units = "in") #PDF

cowplot::plot_grid( #Gridstart.
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
    geom_line( color = "#D95F02" ) +
    labs(y = expression(R^2), x = expression("||"* bold("\u03B2") *"||")) +
    theme(text=element_text(size=14,  family="serif")) +
    ggtitle("A"),
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
  ggplot(tibble( dev.ratio = lassoRes$dev.ratio, #4.Fig
                 lamda = log(lassoRes$lambda),
                 l1_norm = colSums(abs(lassoRes$beta)),
                 df = lassoRes$df), 
         aes( y = dev.ratio , x = l1_norm )) +
    geom_line(data = linesDf,
              aes( y = value, lty = Var1, color = posCoef) , show.legend = F) +
    labs(y = expression(beta), x = expression("||"* bold("\u03B2") *"||")) + 
    coord_cartesian(ylim = c(-1.5,1.5)) +
    scale_y_continuous(sec.axis = sec_axis( ~.*1, "")) +
    scale_color_manual(values = c("#7570B3", "#7570B3")) + 
    geom_abline(intercept = 0, color = "black", slope = 0) + 
    theme(text=element_text(size=14,  family="serif")) +
    ggtitle("B"), 
  
  
  align = c("v","h"), nrow = 2 #Cowplot settings.
) #GridEnd.

dev.off(); #PDF end

sort(apply(as.matrix(t(lassoRes$beta)), FUN = function(x) sum(x == 0), MARGIN = 2)
)

