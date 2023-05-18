# Spline 'cutoff'

bends <- 2:40

# For all
modelList <- list()
for ( i in bends ) { # Fit models, make list of models
  
  model <- lm(data = df,
   formula = BDIsum ~ bs(x = PQBsum_cust,
                         intercept = F, 
                         degree = 1, 
                         knots = i)) 

  modelList[[ i - 1 ]] <- model
  }


for (i in c(10,14,16,18,20,22,24,26,28,30)) { # Some plots
  plot(y = df$BDIsum, x = df$PQBsum_cust)
  splinePreds <- predict(modelList[[ i ]], 
                         newdata = data.frame(PQBsum_cust = 0:80),
                         col = i-1)
  lines(x = 0:80, y = splinePreds)
  readline("Enter for next plot.")
}

which.max(sapply(modelList, AIC))

# Males

modelListMale <- list()
for ( i in bends ) { # Fit models, make list of models
  
  model <- lm(data = df[df$sex == "Male",],
              formula = BDIsum ~ bs(x = PQBsum_cust,
                                    intercept = F, 
                                    degree = 1, 
                                    knots = i)) 
  
  modelListMale[[ i - 1 ]] <- model
}
sapply(modelListMale, AIC)
which.min(sapply(modelListMale, AIC))


# Females
modelListFemale <- list()
for ( i in bends ) { # Fit models, make list of models
  
  model <- lm(data = df[df$sex == "Female",],
              formula = BDIsum ~ bs(x = PQBsum_cust,
                                    intercept = F, 
                                    degree = 1, 
                                    knots = i)) 
  
  modelListFemale[[ i - 1 ]] <- model
}
sapply(modelListFemale, AIC)
which.min(sapply(modelListFemale, AIC))
