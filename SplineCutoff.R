# Spline 'cutoff'

bends <- 1:40

# For all
modelList <- list()
for ( i in bends ) { # Fit models, make list of models
  
  model <- lm(data = df,
   formula = BDIsum ~ bs(x = PQBsum_cust,
                         intercept = F, 
                         degree = 1, 
                         knots = i)) 

  modelList[[ i  ]] <- model
  }

bestOverall <- modelList[[which.min(sapply(modelList, AIC))]] # Choose by AIC

# Males

modelListMale <- list()
for ( i in bends ) { # Fit models, make list of models
  
  model <- lm(data = df[df$sex == "Male",],
              formula = BDIsum ~ bs(x = PQBsum_cust,
                                    intercept = F, 
                                    degree = 1, 
                                    knots = i)) 
  
  modelListMale[[ i ]] <- model
}

bestMale <- modelListMale[[which.min(sapply(modelListMale, AIC))]] # Choose by AIC


# Females
modelListFemale <- list()
for ( i in bends ) { # Fit models, make list of models
  
  model <- lm(data = df[df$sex == "Female",],
              formula = BDIsum ~ bs(x = PQBsum_cust,
                                    intercept = F, 
                                    degree = 1, 
                                    knots = i)) 
  
  modelListFemale[[ i ]] <- model
}

bestFemale <- modelListFemale[[which.min(sapply(modelListFemale, AIC))]] # Choose by AIC

# Plot ----

SplinePlot <- function() {
plot(y = df$BDIsum, x = df$PQBsum_cust, 
     main = "Spline cutoffs",
     ylab = "BDI sum",
     xlab = "PQB sum (no Grandiosity)")
pred <- cbind(predict(bestOverall, newdata = data.frame(PQBsum_cust = 0:90)),
              predict(bestMale, newdata = data.frame(PQBsum_cust = 0:90)),
              predict(bestFemale, newdata = data.frame(PQBsum_cust = 0:90)))

sapply(1:3, function (x) lines(y = pred[,x], 
                               x = 0:90, 
                               lty = x, lwd = 1.5))
legend(x = 80, y = 10, 
       legend = c("Overall", "Male", "Female"), 
       lty = 1:3, box.col = "white") }





