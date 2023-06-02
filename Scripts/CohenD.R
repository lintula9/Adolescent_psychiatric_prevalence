# Cohen's D for each PQ-B cutoff.



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
       ylim = c( -1 , 2 ), 
       lty = 1,
       main = "")
  lines(type = "b",
        y = maleRes[ 2, ],
        x = PQBcutoffs, 
        pch = 0,
        col = cols["Male"],
        lty = 2)
  lines(type = "b",
        y = femaleRes[ 2, ],
        x = PQBcutoffs, 
        pch = 2, lty = 3,
        col = cols["Female"])
  legend(x = 35, 
         y = -.38, 
         cex = 1.5,
         legend = c("Overall", "Male", "Female"), 
         col = cols, 
         lty = 1:3, 
         pch = c(1, 0, 2), 
         box.col = "white")
}
