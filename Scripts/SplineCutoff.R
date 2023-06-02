# Spline 'cutoff'

bends <- 1:40
par(family = "serif")
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
     main = "", fontsize = 2, cex.lab = 1.5,
     ylab = "BDI sum",
     xlab = "PQB distress score",
     col = cols[as.character(df$sex)])
pred <- cbind(predict(bestOverall, newdata = data.frame(PQBsum_cust = 0:90)),
              predict(bestMale, newdata = data.frame(PQBsum_cust = 0:90)),
              predict(bestFemale, newdata = data.frame(PQBsum_cust = 0:90)))

sapply(1:3, function (x) lines(y = pred[,x], 
                               x = 0:90, 
                               col = cols[x], 
                               lwd = 2,
                               lty = x))
legend(x = 70, y = 10, 
       legend = c("Overall", "Male", "Female"), 
       col = cols, 
       cex = 1.5,
       lty = 1:3,
       box.col = "white") }

# All cutoffs plots ----

SplinePlotAll <- function( ) {
  
  plot(y = df$BDIsum, 
       x = df$PQBsum_cust, 
       main = "Spline cutoffs using all data",
       ylab = "BDI sum",
       xlab = "PQB cutoff", 
       col = "black" )
  
  preds <- sapply( modelList, FUN = function( x ) predict( x, 
                                                               newdata = data.frame( PQBsum_cust = 0:90 ) ) )
  sapply( 1 : ncol( preds ) , function( x ) lines(
    y = preds[ , x ],
    x = 0 : 90,
    col = "black"
  ))
}

SplinePlotMale <- function( ) {
  
  plot(y = df[ df$sex == "Male", ]$BDIsum, 
       x = df[ df$sex == "Male", ]$PQBsum_cust, 
       main = "Spline cutoffs for males",
       ylab = "BDI sum",
       xlab = "PQB cutoff", 
       col = "blue" )
  
  preds <- sapply( modelListMale, FUN = function( x ) predict( x, 
                                                            newdata = data.frame( PQBsum_cust = 0:90 ) ) )
  sapply( 1 : ncol( preds ) , function( x ) lines(
    y = preds[ , x ],
    x = 0 : 90,
    col = "blue"
  ))
  }

SplinePlotFemale <- function( ) {
  
  plot(y = df[ df$sex == "Female", ]$BDIsum, 
       x = df[ df$sex == "Female", ]$PQBsum_cust, 
       main = "Spline cutoffs for males",
       ylab = "BDI sum",
       xlab = "PQB cutoff", 
       col = "red" )
  
  preds <- sapply( modelListFemale, FUN = function( x ) predict( x, 
                                                               newdata = data.frame( PQBsum_cust = 0:90 ) ) )
  sapply( 1 : ncol( preds ) , function( x ) lines(
    y = preds[ , x ],
    x = 0 : 90,
    col = "red"
  ))
}

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


# Comparisons to simple linear regression models -----

anova(
  lm(BDIsum ~ PQBsum_cust, data = df),
  bestOverall)


