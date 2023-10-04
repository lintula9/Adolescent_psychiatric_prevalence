

# Diagnostic Odds-Ratio (DOR) calculations.

PQBcuts <- 1:70
# BDImild & BDIMDD.

#BDI mild ---------
DORES_mild <- matrix(0,ncol = 7, nrow = length(PQBcuts))
colnames(DORES_mild) <- c("logDOR","DOR", "Upper", "Lower", "SE", "n_TP", "n_FP")
for ( i in PQBcuts ) {
  with(
    na.omit(df[,c("BDImild", "PQBsum_cust")]), expr = {
         
       restable <- table( BDImild, PQBsum_cust >= i )
       TP <- restable[2,2]
       FP <- restable[1,2]
       TN <- restable[1,1]
       FN <- restable[2,1]
       DOR <- ( TP / FP ) / ( FN / TN )
       logDOR <- log(DOR)
       SElogDOR <- sqrt( (1/TP) + (1/FN) + (1/FP) + (1/TN) )
       DORES_mild[ i , c("logDOR", "DOR", "Upper", "Lower", "SE", "n_TP", "n_FP")] <<- c(logDOR, DOR, exp( logDOR + 1.96*SElogDOR ), exp( logDOR - 1.96*SElogDOR ), exp( SElogDOR ),  restable[2,2], restable[1,2]) }
       
  )
}

DORES_mild_male <- matrix(0,ncol = 7, nrow = length(PQBcuts))
colnames(DORES_mild_male) <- c("logDOR","DOR", "Upper", "Lower", "SE", "n_TP", "n_FP")
for ( i in PQBcuts ) {
  tryCatch(expr = {with(
    na.omit(df[df$sex == "Male", c("BDImild", "PQBsum_cust")]), expr = {
      
      restable <- table( BDImild, PQBsum_cust >= i )
      TP <- restable[2,2]
      FP <- restable[1,2]
      TN <- restable[1,1]
      FN <- restable[2,1]
      DOR <- ( TP / FP ) / ( FN / TN )
      logDOR <- log(DOR)
      SElogDOR <- sqrt( (1/TP) + (1/FN) + (1/FP) + (1/TN) )
      DORES_mild_male[ i , c("logDOR", "DOR", "Upper", "Lower", "SE", "n_TP", "n_FP")] <<- c(logDOR, DOR, exp( logDOR + 1.96*SElogDOR ), exp( logDOR - 1.96*SElogDOR ), exp( SElogDOR ),  restable[2,2], restable[1,2]) }
    
  )}, error = function(e) print("Some errors in BDI mild loops..."))
}


DORES_mild_female <- matrix(0,ncol = 7, nrow = length(PQBcuts))
colnames(DORES_mild_female) <- c("logDOR","DOR", "Upper", "Lower", "SE", "n_TP", "n_FP")
for ( i in PQBcuts ) {
  with(
    na.omit(df[ df$sex == "Female" ,c("BDImild", "PQBsum_cust")]), expr = {
      
      restable <- table( BDImild, PQBsum_cust >= i )
      TP <- restable[ 2, 2 ]
      FP <- restable[ 1, 2 ]
      TN <- restable[ 1, 1 ]
      FN <- restable[ 2, 1 ]
      DOR <- ( TP / FP ) / ( FN / TN )
      logDOR <- log( DOR )
      SElogDOR <- sqrt( (1 / TP ) + ( 1 / FN ) + ( 1 / FP ) + ( 1 / TN ) )
      DORES_mild_female[ i , c("logDOR", "DOR", "Upper", "Lower", "SE", "n_TP", "n_FP")] <<- c(logDOR, DOR, exp( logDOR + 1.96*SElogDOR ), exp( logDOR - 1.96*SElogDOR ), exp( SElogDOR ),  restable[2,2], restable[1,2]) }
    
  )
}

DORplot_mild <- function( ) {
  
  plot(as.vector(DORES_mild[,"DOR"]), 
       x = PQBcuts, type = "b", ylim = c(0,20),
       col = cols["Total"], 
       lty = 3, lwd = 1.5, pch = 2,
       ylab = "", xlab = "",
       cex.axis = 1.5)
  lines(as.vector(DORES_mild_female[,"DOR"]), 
        x = PQBcuts, type = "b", 
        col = cols["Female"], 
        lty = 2, lwd = 1.5, pch = 0)
  lines(as.vector(DORES_mild_male[,"DOR"]), 
        x = PQBcuts, type = "b", 
        col = cols["Male"], 
        lty = 1, lwd = 1.5, pch = 1)
  title(ylab = "DOR", 
        xlab = "PQ-B cut-off", 
        cex.lab = 1.5 )
  legend(x = 55, y = 18, 
         legend = c("Overall", "Male", "Female"), 
         col = cols, 
         lty = 1:3, 
         cex = 1.5,
         pch = c(1, 0, 2), 
         box.col = "white")
  title( main = "" ) 
}

DORplot_mild_CIs <- function() {
  
  par(mfrow = c(3,1))
  j <- 1
  for (i in list(DORES_mild, DORES_mild_male, DORES_mild_female)) {
    plotnames <- c("Total", "Male", "Female")
    plot(i[,"DOR"], ylim = c(0,40), xlab = "PQ-B cut-off", ylab = "DOR and 95% CI", main = plotnames[j], col = cols[j])
    segments( x0 = PQBcuts, y0 = i[ , "Lower" ], y1 = i[ , "Upper" ], col = cols[j] )
    j <- j + 1
    }
  
  par(mfrow = c(1,1))}


#BDI MDD ---------

DORES_MDD <- matrix(0,ncol = 7, nrow = length(PQBcuts))
colnames(DORES_MDD) <- c("logDOR","DOR", "Upper", "Lower", "SE", "n_TP", "n_FP")
for ( i in PQBcuts ) {
  with(
    na.omit(df[,c("BDIMDD", "PQBsum_cust")]), expr = {
      
      restable <- table( BDIMDD, PQBsum_cust >= i )
      TP <- restable[2,2]
      FP <- restable[1,2]
      TN <- restable[1,1]
      FN <- restable[2,1]
      DOR <- ( TP / FP ) / ( FN / TN )
      logDOR <- log(DOR)
      SElogDOR <- sqrt( (1/TP) + (1/FN) + (1/FP) + (1/TN) )
      DORES_MDD[ i , c("logDOR", "DOR", "Upper", "Lower", "SE", "n_TP", "n_FP")] <<- c(logDOR, DOR, exp( logDOR + 1.96*SElogDOR ), exp( logDOR - 1.96*SElogDOR ), exp( SElogDOR ),  restable[2,2], restable[1,2]) }
    
  )
}

DORES_MDD_male <- matrix(0,ncol = 7, nrow = length(PQBcuts))
colnames(DORES_MDD_male) <- c("logDOR","DOR", "Upper", "Lower", "SE", "n_TP", "n_FP")
for ( i in PQBcuts ) {
  tryCatch(
    expr = {with(
    na.omit(df[df$sex == "Male", c("BDIMDD", "PQBsum_cust")]), expr = {
      
      restable <- table( BDIMDD, PQBsum_cust >= i )
      TP <- restable[2,2]
      FP <- restable[1,2]
      TN <- restable[1,1]
      FN <- restable[2,1]
      DOR <- ( TP / FP ) / ( FN / TN )
      logDOR <- log(DOR)
      SElogDOR <- sqrt( (1/TP) + (1/FN) + (1/FP) + (1/TN) )
      DORES_MDD_male[ i , c("logDOR", "DOR", "Upper", "Lower", "SE", "n_TP", "n_FP")] <<- c(logDOR, DOR, exp( logDOR + 1.96*SElogDOR ), exp( logDOR - 1.96*SElogDOR ), exp( SElogDOR ),  restable[2,2], restable[1,2]) }
    
  )}, error = function(e) print("Some errors in for lopps..")
  )
}


DORES_MDD_female <- matrix(0,ncol = 7, nrow = length(PQBcuts))
colnames(DORES_MDD_female) <- c("logDOR","DOR", "Upper", "Lower", "SE", "n_TP", "n_FP")
for ( i in PQBcuts ) {
  with(
    na.omit(df[ df$sex == "Female" ,c("BDIMDD", "PQBsum_cust")]), expr = {
      
      restable <- table( BDIMDD, PQBsum_cust >= i )
      TP <- restable[ 2, 2 ]
      FP <- restable[ 1, 2 ]
      TN <- restable[ 1, 1 ]
      FN <- restable[ 2, 1 ]
      DOR <- ( TP / FP ) / ( FN / TN )
      logDOR <- log( DOR )
      SElogDOR <- sqrt( (1 / TP ) + ( 1 / FN ) + ( 1 / FP ) + ( 1 / TN ) )
      DORES_MDD_female[ i , c("logDOR", "DOR", "Upper", "Lower", "SE", "n_TP", "n_FP")] <<- c(logDOR, DOR, exp( logDOR + 1.96*SElogDOR ), exp( logDOR - 1.96*SElogDOR ), exp( SElogDOR ),  restable[2,2], restable[1,2]) }
    
  )
}

DORplot_MDD <- function( ) {
  
  plot(as.vector(DORES_MDD[,"DOR"]), 
       x = PQBcuts, type = "b", ylim = c(0,20),
       col = cols["Total"], 
       lty = 3, lwd = 1.5, pch = 2,
       ylab = "", xlab = "",
       cex.axis = 1.5)
  lines(as.vector(DORES_MDD_female[,"DOR"]), 
        x = PQBcuts, type = "b", 
        col = cols["Female"], 
        lty = 2, lwd = 1.5, pch = 0)
  lines(as.vector(DORES_MDD_male[,"DOR"]), 
        x = PQBcuts, type = "b", 
        col = cols["Male"], 
        lty = 1, lwd = 1.5, pch = 1)
  title(ylab = "DOR", 
        xlab = "PQ-B cut-off", 
        cex.lab = 1.5 )
  legend(x = 55, y = 18, 
         legend = c("Overall", "Male", "Female"), 
         col = cols, 
         lty = 1:3, 
         cex = 1.5,
         pch = c(1, 0, 2), 
         box.col = "white")
  title( main = "" ) 
}

DORplot_MDD_CIs <- function() {
  
  par(mfrow = c(3,1))
  j <- 1
  for (i in list( DORES_MDD, DORES_MDD_male, DORES_MDD_female )) {
    plotnames <- c("Total", "Male", "Female" )
    plot(i[,"DOR"], ylim = c(0,40), xlab = "PQ-B cut-off", ylab = "DOR and 95% CI", main = plotnames[j], col = cols[j])
    segments( x0 = PQBcuts, y0 = i[ , "Lower" ], y1 = i[ , "Upper" ], col = cols[j] )
    j <- j + 1
  }
  
  par(mfrow = c(1,1))}

