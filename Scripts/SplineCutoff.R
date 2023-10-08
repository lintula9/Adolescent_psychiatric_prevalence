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
     main = "", fontsize = 2, cex.lab = 1.5,
     ylab = "BDI sum",
     xlab = "PQ-B distress score",
     col = cols[as.character(df$sex)],
     cex.axis = 2,
     cex.lab = 2)
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
       lwd = 3,
       lty = 1:3,
       box.col = "white") 
  }

# All cutoffs plots ----

SplinePlotAll <- function( ) {
  
  plot(y = df$BDIsum, 
       x = df$PQBsum_cust, 
       main = "Spline cutoffs using all data",
       ylab = "BDI sum",
       xlab = "PQB cutoff", 
       col = "black",
       cex.axis = 1.5)
  
  preds <- sapply( modelList, FUN = function( x ) predict( x, 
                                                               newdata = data.frame( PQBsum_cust = 0:90 ) ) )
  
  bfs <- bic_to_bf(sapply(modelList, BIC), denominator = sapply(modelList, BIC)[1], log = F)
  alphas <- (sapply(bfs, FUN = function(x) exp( x ) / sum(exp(bfs)))) 
  alphas <- alphas * (1 / max(alphas))
  
  sapply( 1 : ncol( preds ) , function( x ) lines(
    y = preds[ , x ],
    x = 0 : 90,
    col = scales::alpha( cols[ 1 ], alphas[ x ])
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

# LogLik plots ------

SplineLLplots <- function(){
  plot(sapply(modelList, logLik), 
       ylab = "log likelihood", 
       xlab = "PQB elbow point",
       main = "Overall",
       col = cols["Total"], 
       cex.lab = 1.5, cex.axis = 1.5)
  abline(lty = "dashed", 
         v = which.max(sapply(modelList, logLik)))
  plot(sapply(modelListMale, logLik), 
       ylab = "log likelihood", 
       xlab = "PQB elbow point",
       main = "Male",
       col = cols["Male"], 
       cex.lab = 1.5, cex.axis = 1.5)
  abline(lty = "dashed", 
         v = which.max(sapply(modelListMale, logLik)))
  plot(sapply(modelListFemale, logLik), 
       ylab = "log likelihood", 
       xlab = "PQB elbow point",
       main = "Female",
       col = cols["Female"], 
       cex.lab = 1.5, cex.axis = 1.5)
  abline(lty = "dashed", 
         v = which.max(sapply(modelListFemale, logLik)))
}

# Comparisons to simple linear regression models -----

anova(
  lm(BDIsum ~ PQBsum_cust, data = df),
  bestOverall)
anova(
  lm(BDIsum ~ PQBsum_cust, data = df[df$sex == "Male", ]),
  bestMale)
anova(
  lm(BDIsum ~ PQBsum_cust, data = df[df$sex == "Female", ]),
  bestFemale)

summary(lm(BDIsum ~ PQBsum_cust, data = df))$r.squared - summary(bestOverall)$r.squared
summary(lm(BDIsum ~ PQBsum_cust, data = df[df$sex == "Male", ]))$r.squared - summary(bestMale)$r.squared
summary(lm(BDIsum ~ PQBsum_cust, data = df[df$sex == "Female", ]))$r.squared - summary(bestFemale)$r.squared

summary(lm(BDIsum ~ PQBsum_cust, data = df))$r.squared 
summary(lm(BDIsum ~ PQBsum_cust, data = df[df$sex == "Male", ]))$r.squared
summary(lm(BDIsum ~ PQBsum_cust, data = df[df$sex == "Female", ]))$r.squared

summary(lm(BDIsum ~ PQBsum_cust * factor(sex), data = df))



# Calculate slopes: ------
# Nah.

# Test


