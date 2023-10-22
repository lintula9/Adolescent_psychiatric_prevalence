# Canonical Correlation Analysis CCA

ccd <- scale(as.matrix( na.omit( df[ , c( PQBvars, BDIvars ) ] ) ))

if (F){ # This takes long.

ccaLambdas <- CCA::estim.regul( ccd[ , PQBvars ], # loocv and grid search for finding lambdas.
                                ccd[ , BDIvars ], 
                                grid1 = seq( .64 , .70 , length = 10 ), # First run suggested ~ 0.67, 0.56. Second 0.68, 0.57.
                                grid2 = seq( .53 , .6 , length = 10 ) )

saveRDS( ccaLambdas, file = "CCAloocvLambdaSearch.R"  ) # file is retrievable by this name.

}
if(F) {
  readRDS("CCAloocvLambdaSearch.R") # Fetch ccaLambdas loocv result.
  }

CCAResult <- CCA::rcc( ccd[ , PQBvars_cust ], 
                       ccd[ , BDIvars ], 
                       lambda1 = ccaLambdas$lambda1 , lambda2 = ccaLambdas$lambda2 )

# Look if the most correlated structures in data are due to PQ-B 8, PQ-B 18?
write.csv( x = cbind(CCAResult$xcoef[,1:3], CCAResult$ycoef[,1:3]) , 
           file = "CCAresult.csv" )

round(CCAResult$xcoef[,1:3], 2)              
round(CCAResult$ycoef[,1:3], 2)
CCAResult$cor



