# Canonical Correlation Analysis CCA

ccd <- scale(as.matrix( na.omit( df[ , c( PQBvars, BDIvars ) ] ) ))
colnames(ccd) <- c(PQBvars, BDIvars)
if (F){ # This takes long.

ccaLambdas <- CCA::estim.regul( ccd[ , PQBvars ], # loocv and grid search for finding lambdas.
                                ccd[ , BDIvars ], 
                                grid1 = seq( .64 , .70 , length = 10 ), # First run suggested ~ 0.67, 0.56. Second 0.68, 0.57.
                                grid2 = seq( .53 , .6 , length = 10 ) )

saveRDS( ccaLambdas, file = "CCAloocvLambdaSearch.R"  ) # file is retrievable by this name.

}
if(F) {
  ccaLambdas <- readRDS("CCAloocvLambdaSearch.R") # Fetch ccaLambdas loocv result.
  }

CCAResult <- CCA::rcc( ccd[ , PQBvars ], 
                       ccd[ , BDIvars ], 
                       lambda1 = ccaLambdas$lambda1 , lambda2 = ccaLambdas$lambda2 )
if(F) {write.csv( x = do.call(cbind, list(CCAResult$xcoef[,1:3], 
                                          CCAResult$scores$corr.X.xscores[,1:3], 
                                          CCAResult$ycoef[,1:3],
                                          CCAResult$scores$corr.Y.xscores[,1:3])) , 
           file = "CCAresult.csv" )} # Save Result as csv.


# Look if the most correlated structures in data are due to PQ-B 8, PQ-B 18?

round(CCAResult$xcoef[,1:3], 2) # Regression coefficients.              
round(CCAResult$ycoef[,1:3], 2)
CCAResult$scores$corr.X.xscores # Correlations to respective canoncical component.
CCAResult$scores$corr.Y.xscores




plt.cc(CCAResult)

