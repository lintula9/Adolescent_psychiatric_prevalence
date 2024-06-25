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


# BDI CFA for Steiner some1? -- ----
# Promax rotations:
efaRes21b <- efa(df[ , BDIvars], ordered = T, nfactors = 1, rotation = "promax")
efaRes22b <- efa(df[ , BDIvars], ordered = T, nfactors = 2, rotation = "promax")
efaRes23b <- efa(df[ , BDIvars], ordered = T, nfactors = 3, rotation = "promax")
round(efaRes23$loadings - efaRes23b$loadings, digits = 1)


model_stein <- '
Cognitive =~ BDI1 + BDI2 + BDI3 + BDI5 + BDI6 + BDI7 + BDI8 + BDI9 + BDI14
SomaticAffective =~ BDI4 + BDI10 + BDI11 + BDI12 + BDI13 + BDI15 + BDI17
GuiltPunishment =~ BDI18 + BDI19 + BDI20

Cognitive ~~ SomaticAffective + GuiltPunishment
SomaticAffective ~~ GuiltPunishment
' # BDI Insomnia as well as BDI sexual life are left out due to small loadings.
stein_res <- sem(model_stein, data = df[,BDIvars], ordered = T, estimator = "WLSMV")
lavInspect(stein_res, "std")$psi
summary(stein_res)
fitmeasures(stein_res)


