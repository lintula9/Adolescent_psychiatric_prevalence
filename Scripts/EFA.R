# EFA for PQ-B

efaRes <- efa(df[ , PQBvars], ordered = T, nfactors = 5, rotation = "oblimin")
with(efaRes, {
  loadings[ loadings < .3] <- 0
  print(loadings)
  
  
})
