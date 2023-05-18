# Lasso paths
lassoRegDat <- na.omit(df[,c("BDIsum", PQBvars_cust)])
pdf("LassoPaths.pdf", fonts = "serif", compress = F, pointsize = 8)

LassoPlot <- function() {plot(glmnet(y = lassoRegDat$BDIsum, 
     as.matrix(x = lassoRegDat[,PQBvars_cust])), 
     label = T, 
     xvar = "dev", 
     main = "Lasso regularization paths: all PQ-B variables predicting BDI sum") }
 
dev.off()
