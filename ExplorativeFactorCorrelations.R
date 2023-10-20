# Explorative factor correlative analysis:

ExplorativeModel <- '

# efa block 1, without PQB 7 or PQB 12
    efa("efa1")*P1 + 
    efa("efa1")*P2 +
    efa("efa1")*P3 =~ PQB1 + PQB2 + PQB3 + PQB4 + PQB5 + PQB6 + PQB8 + PQB9 + 
    PQB10 + PQB11 + PQB13 + PQB14 + PQB15 + 
    PQB16 + PQB17 + PQB18 + PQB19 + PQB20 + PQB21
    
# efa block 1
    efa("efa2")*D1 + 
    efa("efa2")*D2 + 
    efa("efa2")*D3 =~ BDI1 + BDI2 + BDI3 + BDI4 + BDI5 + BDI6 + BDI7 + BDI8 + BDI9 + BDI10 + 
    BDI11 + BDI12 + BDI13 + BDI14 + BDI15 + BDI16 + BDI17 + BDI18 + BDI19 + BDI20 + BDI21

# Factor correlations


'

explorativeGraph <- sem(ExplorativeModel, data = df[,c(PQBvars_cust, BDIvars)],
                        estimator = "WLSMV", ordered = T, rotation = "geomin")

LV_network <- solve(lavInspect(explorativeGraph, "est")$psi)
diag(LV_network) <- 0
qgraph(lavInspect(explorativeGraph, "est")$psi, edge.labels = T)


# Drop out PQ-B 7 and 12.
"PLEs on PQ-B are additionally divided into sub-scales of 
Sensory Distortion (items 2, 3, 9, 10, 17, 19, 20), 
Grandiosity (item 7), 
Disorganised Speech (items 6, 21), Paranoia (items 8, 18) and 
Unusual Thoughts (items 1, 4, 5, 11, 12, 13, 14, 15, 16) sub-scales, 
which are derived from Structured interview for Prodromal Syndromes (SIPS)-instrument (Loewy et al., 2007)."

modelStructure <- matrix(c(), ncol = )
library(psychonetrics)
lnm(latent = "ggm", 
    )