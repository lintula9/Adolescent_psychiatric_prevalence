# Correlations for review
write.csv(x = lavCor(df[ , c(BDIvars, PQBvars_cust)], ordered = T), file = "polycor.csv")
write.csv(x = lavCor(df[ , c(BDIvars, PQBvars_cust)], ordered = T, output = "th"), file = "polycor_thresholds.csv")
write.csv(x = cor(df[ , c(BDIvars, PQBvars_cust)], method = "spearman", use = "complete.obs"), file = "spearman.csv")
subscales_and_sums = c(names(df)[grep("PQB[^0-9]",names(df))][c(11,1:5)],
                       names(df)[grep("BDI",names(df))][43:46])
write.csv(x = cor(df[ , subscales_and_sums ], use = "complete.obs"), 
          file = "sums_and_subscales.csv")

print(corr.test(df[ , subscales_and_sums], use = "complete.obs"), short = F)

