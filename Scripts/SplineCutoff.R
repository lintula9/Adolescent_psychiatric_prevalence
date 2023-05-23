# Spline 'cutoff'

bends <- 1:70

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
                               col = c("black","blue","red")[x], 
                               lwd = 2))
legend(x = 70, y = 10, 
       legend = c("Overall", "Male", "Female"), 
       col = c("black","blue","red"),
       lty = 1,
       box.col = "white") }


# AIC plots -----

SplineAICplots <- function(){
  plot(sapply(modelList, AIC), 
       ylab = "AIC", 
       xlab = "PQB sum bend point",
       main = "Overall",
       col = "black")
  abline(lty = "dashed", 
         v = which.min(sapply(modelList, AIC)))
  plot(sapply(modelListMale, AIC), 
       ylab = "AIC", 
       xlab = "PQB sum bend point",
       main = "Male",
       col = "blue")
  abline(lty = "dashed", 
         v = which.min(sapply(modelListMale, AIC)))
  plot(sapply(modelListFemale, AIC), 
       ylab = "AIC", 
       xlab = "PQB sum bend point",
       main = "Female",
       col = "red",)
  abline(lty = "dashed", 
         v = which.min(sapply(modelListFemale, AIC)))
}


