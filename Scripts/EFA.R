# EFA for PQ-B
if(!exists("package_names")) source("scripts/data_and_packages.R")

efaRes <- efa(df[ , PQBvars], ordered = T, nfactors = 5, rotation = "oblimin")
with(efaRes, {
  loadings[ loadings < .3] <- 0
  print(loadings)
  print(fitMeasures(efaRes, fit.measures = c("SRMR","SRMR.scaled","CFI.scaled", "RMSEA.scaled")))
  
  
})

efaRes21 <- efa(df[ , BDIvars], ordered = T, nfactors = 1, rotation = "oblimin")
efaRes22 <- efa(df[ , BDIvars], ordered = T, nfactors = 2, rotation = "oblimin")
efaRes23 <- efa(df[ , BDIvars], ordered = T, nfactors = 3, rotation = "oblimin")

for (i in list(efaRes21, efaRes22, efaRes23)) {with(i, {
  loadings[ loadings < .3] <- 0
  print(loadings)})}
for (i in list(efaRes21, efaRes22, efaRes23)) {with(i, {
  fits <- fitMeasures(i, fit.measures = c("SRMR","SRMR.scaled","CFI.scaled", "RMSEA.scaled"))
  print(fits)})}
