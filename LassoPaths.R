# Lasso paths
lassoRegDat <- na.omit(df[,c("BDIsum", PQBvars_cust)])
pdf("LassoPaths.pdf", fonts = "serif", compress = F, pointsize = 8)
plot(glmnet(y = lassoRegDat$BDIsum, 
     as.matrix(x = lassoRegDat[,PQBvars_cust])), label = T, xvar = "dev")
plot(glmnet(y = lassoRegDat$BDIsum, 
            as.matrix(x = lassoRegDat[,PQBvars_cust])), label = T, xvar = "norm")
dev.off()
