############### ############### ############### ############### ############### 
############### COHEN'S D IS LEFT OUT OF ANALYSIS 3.10.2023 ###############
############### ############### ############### ############### ############### 


# Set cutoffs, as in superiorities.

PQBcutoffs <- 1:40

par(family = "serif")
# Function for D calculations.

cohenPlot <- function( publication = F ) {

  allRes <- sapply(PQBcutoffs, 
                   FUN = function(x) cohen.d(df$BDIsum, 
                                             group = df$PQBsum_cust >= x)$cohen.d) 
  maleRes <- sapply(PQBcutoffs, 
                   FUN = function(x) cohen.d(df[df$sex == "Male",]$BDIsum, 
                                             group = df[df$sex == "Male",]$PQBsum_cust >= x)$cohen.d)
  femaleRes <- sapply(PQBcutoffs, 
                   FUN = function(x) cohen.d(df[df$sex == "Female",]$BDIsum, 
                                             group = df[df$sex == "Female",]$PQBsum_cust >= x)$cohen.d) 

  plot(allRes[ 2, ], 
       type = "b", 
       ylab = "Cohen's D", 
       xlab = "PQ-B sum cutoff",
       ylim = c(-1,2), main = "")
  segments(x0 = PQBcutoffs,x1 = PQBcutoffs,
           y0 = allRes[ 1 , ], y1 = allRes[ 3 , ])
  title(main = "BDI difference effect sizes for all")
  
  plot(maleRes[ 2, ], 
       type = "b", 
       ylab = "Cohen's D", 
       xlab = "PQ-B sum cutoff",
       ylim = c(-1,2), col = "blue")
  segments(x0 = PQBcutoffs,x1 = PQBcutoffs,
           y0 = maleRes[ 1 , ], y1 = maleRes[ 3 , ], col = "blue")
  title(main = "BDI difference effect sizes for males")
  
  plot(femaleRes[ 2, ], 
       type = "b", 
       ylab = "Cohen's D", 
       xlab = "PQ-B sum cutoff",
       ylim = c(-1,2), col = "red")
  segments(x0 = PQBcutoffs,x1 = PQBcutoffs,
           y0 = femaleRes[ 1 , ], y1 = femaleRes[ 3 , ], col = "red")
  title(main = "BDI effect sizes for females")
  
  }

# Combined 
cohenPlotCombined <- function() {
  
  allRes <- sapply(PQBcutoffs, 
                   FUN = function(x) cohen.d(df$BDIsum, 
                                             group = df$PQBsum_cust >= x)$cohen.d) 
  maleRes <- sapply(PQBcutoffs, 
                    FUN = function(x) cohen.d(df[df$sex == "Male",]$BDIsum, 
                                              group = df[df$sex == "Male",]$PQBsum_cust >= x)$cohen.d)
  femaleRes <- sapply(PQBcutoffs, 
                      FUN = function(x) cohen.d(df[df$sex == "Female",]$BDIsum, 
                                                group = df[df$sex == "Female",]$PQBsum_cust >= x)$cohen.d) 
  
  plot(allRes[ 2 , ], 
       type = "b", 
       ylab = "Cohen's d", 
       xlab = "PQ-B cut-off", 
       cex.lab = 1.5,
       pch = 1, 
       col = cols["Total"],
       ylim = c( 0 , 2 ), 
       lty = 1, lwd = 1.5,
       main = "",
       cex.axis = 1.5)
  lines(type = "b",
        y = maleRes[ 2, ],
        x = PQBcutoffs, 
        pch = 0,
        col = cols["Male"],
        lty = 2, lwd = 1.5)
  lines(type = "b",
        y = femaleRes[ 2, ],
        x = PQBcutoffs, 
        pch = 2, lty = 3,
        col = cols["Female"], lwd = 1.5)
  legend(x = 30, 
         y = .25, 
         cex = 1.5,
         legend = c("Overall", "Male", "Female"), 
         col = cols, 
         lty = 1:3, 
         pch = c(1, 0, 2), 
         box.col = "white")
}


# Highest d values: -----

CohenRes <- lapply(PQBcutoffs, 
                 FUN = function(x) cohen.d(df$BDIsum, 
                                           group = df$PQBsum_cust >= x)) 
CohenResMales <- lapply(PQBcutoffs, 
                  FUN = function(x) cohen.d(df[df$sex == "Male",]$BDIsum, 
                                            group = df[df$sex == "Male",]$PQBsum_cust >= x))
CohenResFemales <- lapply(PQBcutoffs, 
                    FUN = function(x) cohen.d(df[df$sex == "Female",]$BDIsum, 
                                              group = df[df$sex == "Female",]$PQBsum_cust >= x)) 

if(FALSE) {
  
  CohenRes[[which.max(sapply(CohenRes, 
                             FUN = function(x) {x$cohen.d[2]}))]]
  CohenRes[[which.max(sapply(CohenRes, 
                             FUN = function(x) {x$cohen.d[2]}))]]$p
  
  CohenResMales[[which.max(sapply(CohenResMales, FUN = function(x) x$cohen.d[2]))]]
  CohenResMales[[which.max(sapply(CohenRes, FUN = function(x) x$cohen.d[2]))]]$p
  
  
  CohenResFemales[[which.max(sapply(CohenResMales, FUN = function(x) x$cohen.d[2]))]]
  CohenResFemales[[which.max(sapply(CohenRes, FUN = function(x) x$cohen.d[2]))]]$p
  
  
}
