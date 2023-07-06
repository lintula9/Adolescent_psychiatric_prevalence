# Lasso paths
lassoRegDat <- na.omit(df[,c("BDIsum", PQBvars)])
pdf("LassoPaths.pdf", fonts = "serif", compress = F, pointsize = 8)

LassoPlot <- function() {plot(glmnet(y = lassoRegDat$BDIsum, 
     as.matrix(x = lassoRegDat[,PQBvars_cust])), 
     label = T, 
     xvar = "dev", 
     main = "Lasso regularization paths: all PQ-B variables predicting BDI sum") }
LassoPlot()
 
dev.off()


lassoRes <- glmnet(y = lassoRegDat$BDIsum, 
                   as.matrix(x = lassoRegDat[,PQBvars]))
matplot(x=lassoRes$dev.ratio,
        t(lassoRes$beta), type = "l", 
)



ggplot(tibble( dev.ratio = lassoRes$dev.ratio,
               lamda = log(lassoRes$lambda),
               df = lassoRes$df
               ), aes( y = dev.ratio , x = lamda )) +
  geom_line( ) + 
  geom_line( aes( y = df/41 ), lty = "dashed") + 
  labs(y = "BDI variance explained", x = "Amount of regularization") +
  scale_y_continuous(sec.axis = sec_axis( ~.*41, "Number of PQ-B variables included"), 
                     breaks = c(20, 15, 10, 5, 0)/41, labels = rev(c(0, .12, .24, .37, .49))) + 
  guides( col = c("black", "red"),) 

ggplot(tibble( dev.ratio = lassoRes$dev.ratio,
               lamda = log(lassoRes$lambda),
               df = lassoRes$df), aes( y = dev.ratio , x = lamda )) +
  geom_line( aes( y = lassoRes$beta[ 1 , ] ) ) +
  labs(y = "Coefficient value", x = "Amount of regularization")
