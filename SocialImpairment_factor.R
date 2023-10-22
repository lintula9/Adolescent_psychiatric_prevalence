# Social impairment -factor, Psychosis factor, Depression factor

SIFmodel <- '
SocialImpairment =~ PQB8 + PQB18 + BDI14
Psychosis =~  PQB1 + PQB2 + PQB3 + PQB4 + PQB5 + PQB6 + 
              PQB8 + PQB9 + PQB10 + PQB11 + PQB13 + 
              PQB14 + PQB15 + PQB16 + PQB17 + PQB18 + 
              PQB19 + PQB20 + PQB21
Depression =~ BDI1 + BDI2 + BDI3 + BDI4 + BDI5 + BDI6 + BDI7 + BDI8 + BDI9 + BDI10 + 
              BDI11 + BDI12 + BDI13 + BDI14 + 
              BDI15 + BDI16 + BDI17 + BDI18 + BDI19 + BDI20 + BDI21
'
SIFres <- sem(SIFmodel, df[,c(BDIvars, PQBvars_cust)], estimator = "WLSMV", 
              ordered = T, std.lv = T, mimic = "mplus", parameterization = "theta" )
summary(SIFres)

PQB8probsSI <- LavaanIRTProbabilities(SIFres, varname = "PQB8", dimname = "SocialImpairment")
ItemInformation(PQB8probs)
matplot(PQB8probs, type = "l")

plot(ItemInformation(PQB8probsSI), 
     xlab = "Social Impairment level", type = "l")


lines(x = ,ItemInformation(PQB8probsSI), lty = "dashed")


PQB8probsPsychosis <- LavaanIRTProbabilities(SIFres, varname = "PQB8", dimname = "Psychosis")
plot(ItemInformation(PQB8probsPsychosis), type = "l", ylim = c(0, 3))
lines(x = seq(-6, 6, .01),ItemInformation(PQB8probsSI))

# No insights are gained.
