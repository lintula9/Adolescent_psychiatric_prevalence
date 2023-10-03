# Diagnostic Odds-Ratio (DOR) calculations.

PQBcuts <- 1:70
# BDImild & BDIMDD.

#BDI mild ---------
DORES_mild <- list()
for ( i in PQBcuts ) {
  with(na.omit(df[,c("BDImild", "PQBsum_cust")]), expr = 
         DORES_mild[[i]] <<- dor(truth = factor(as.logical(BDImild)), 
                            response = factor(as.logical(PQBsum_cust >= i)), 
                            positive =  "TRUE")
  )
  
}
DORES_mild_female <- list()
for ( i in PQBcuts ) {
  with(na.omit(df[df$sex == "Male",c("BDImild", "PQBsum_cust", "sex")]), expr = 
         DORES_mild_female[[i]] <<- dor(truth = factor(as.logical(BDImild)), 
                                   response = factor(as.logical(PQBsum_cust >= i)), 
                                   positive =  "TRUE")
  )
  
}
DORES_mild_male <- list()
for ( i in PQBcuts ) {
  with(na.omit(df[df$sex == "Female",c("BDImild", "PQBsum_cust", "sex")]), expr = 
         DORES_mild_male[[i]] <<- dor(truth = factor(as.logical(BDImild)), 
                                 response = factor(as.logical(PQBsum_cust >= i)), 
                                 positive =  "TRUE")
  )
  
}

DORplot_mild <- function( ) {
  
  plot(as.vector(DORES_mild), 
       x = PQBcuts, type = "b", ylim = c(0,12),
       col = cols["Total"], 
       lty = 3, lwd = 1.5, pch = 2,
       ylab = "", xlab = "",
       cex.axis = 1.5)
  lines(as.vector(DORES_mild_female), 
        x = PQBcuts, type = "b", 
        col = cols["Female"], 
        lty = 2, lwd = 1.5, pch = 0)
  lines(as.vector(DORES_mild_male), 
        x = PQBcuts, type = "b", 
        col = cols["Male"], 
        lty = 1, lwd = 1.5, pch = 1)
  title(ylab = "DOR", 
        xlab = "PQ-B cut-off", 
        cex.lab = 1.5 )
  legend(x = 30, y = .57, 
         legend = c("Overall", "Male", "Female"), 
         col = cols, 
         lty = 1:3, 
         cex = 1.5,
         pch = c(1, 0, 2), 
         box.col = "white")
  title(main = "") 
  
  
  
}


#BDI MDD ---------
DORES <- list()
for ( i in PQBcuts ) {
  with(na.omit(df[,c("BDIMDD", "PQBsum_cust")]), expr = 
  DORES[[i]] <<- dor(truth = factor(as.logical(BDIMDD)), 
      response = factor(as.logical(PQBsum_cust >= i)), 
      positive =  "TRUE")
  )
  
}
DORES_female <- list()
for ( i in PQBcuts ) {
  with(na.omit(df[df$sex == "Male",c("BDIMDD", "PQBsum_cust", "sex")]), expr = 
         DORES_female[[i]] <<- dor(truth = factor(as.logical(BDIMDD)), 
                            response = factor(as.logical(PQBsum_cust >= i)), 
                            positive =  "TRUE")
  )
  
}
DORES_male <- list()
for ( i in PQBcuts ) {
  with(na.omit(df[df$sex == "Female",c("BDIMDD", "PQBsum_cust", "sex")]), expr = 
         DORES_male[[i]] <<- dor(truth = factor(as.logical(BDIMDD)), 
                            response = factor(as.logical(PQBsum_cust >= i)), 
                            positive =  "TRUE")
  )
  
}

DORplot_MDD <- function( ) {
  
  plot(as.vector(DORES), 
       x = PQBcuts, type = "b", ylim = c(0,12),
       col = cols["Total"], 
       lty = 3, lwd = 1.5, pch = 2,
       ylab = "", xlab = "",
       cex.axis = 1.5)
  lines(as.vector(DORES_female), 
        x = PQBcuts, type = "b", 
        col = cols["Female"], 
        lty = 2, lwd = 1.5, pch = 0)
  lines(as.vector(DORES_male), 
        x = PQBcuts, type = "b", 
        col = cols["Male"], 
        lty = 1, lwd = 1.5, pch = 1)
  title(ylab = "Superiority index", 
        xlab = "PQ-B cut-off", 
        cex.lab = 1.5 )
  legend(x = 30, y = .57, 
         legend = c("Overall", "Male", "Female"), 
         col = cols, 
         lty = 1:3, 
         cex = 1.5,
         pch = c(1, 0, 2), 
         box.col = "white")
  title(main = "") 
  
  
  
}

