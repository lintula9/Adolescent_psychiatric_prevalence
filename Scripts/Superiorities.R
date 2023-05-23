# Probability of superiority

source("scripts/data_and_packages.R")

# BDI Mild -------
PQBcutoffs <- 1:40


# Function that plots, and saves results if necessary.
superiority_plots <- function(PQBcutoffs = PQBcutoffs, BDI, save = F, plots = T) {
  # Both sexes.
  superiorities <- lapply(PQBcutoffs, 
                          FUN = function (x){
    brunnermunzel.test(x = BDI[df$PQBsum_cust < x],
                       y = BDI[df$PQBsum_cust >= x])
    })

  # Male.
  superioritiesMale <- lapply(PQBcutoffs, 
                              FUN = function (x){
    maledat <- df[df$sex == "Male" , ]
    brunnermunzel.test(x = BDI[df$sex == "Male"][maledat$PQBsum_cust < x],
                       y = BDI[df$sex == "Male"][maledat$PQBsum_cust >= x])
    })

  
  # Female.
  superioritiesFemale <- lapply(PQBcutoffs, 
                                FUN = function (x){
    femaledat <- df[df$sex == "Female" , ]
    brunnermunzel.test(x = BDI[df$sex == "Female"][femaledat$PQBsum_cust < x],
                       y = BDI[df$sex == "Female"][femaledat$PQBsum_cust >= x])
    
  })
  
  if (plots) {
  plot(sapply(superioritiesFemale, FUN = function(x) x$estimate), 
       x = PQBcutoffs, type = "b", ylim = c( .5, 1 ), col = "red", ylab = "")
  lines(y = sapply(superioritiesMale, FUN = function(x) x$estimate), 
        x = PQBcutoffs, type = "b", col = "blue", lwd = 1.5)
  lines(y = sapply(superiorities, FUN = function(x) x$estimate), 
        x = PQBcutoffs, type = "b", col = "black", lwd = 1.5)
  title(ylab = "Superiority: P( X < Y ) + .5 * P( X = Y )")
  legend(x = 30, y = .6, 
         legend = c("Overall", "Male", "Female"), 
         col = c("black","blue","red"), 
         lty = 1, 
         pch = 1, 
         box.col = "white")
  title(main = "Superiorities of BDI-21 for different PQ-B cutoffs")
  
  ciAll <- sapply(superiorities, FUN = function(x) x$conf.int)
  plot(sapply(superiorities, FUN = function(x) x$estimate), 
       x = PQBcutoffs, type = "b", ylim = c( .5, 1 ), col = "black", ylab = "")
  segments(x0 = PQBcutoffs, x1 = PQBcutoffs, 
           y0 = ciAll[1,], y1 = ciAll[2,])
  title(main = "Superiorities with 95% CI")
  
  ciMale <- sapply(superioritiesMale, FUN = function(x) x$conf.int)
  plot(sapply(superioritiesMale, FUN = function(x) x$estimate), 
       x = PQBcutoffs, type = "b", ylim = c( .5, 1 ), col = "blue", ylab = "")
  segments(x0 = PQBcutoffs, x1 = PQBcutoffs, 
           y0 = ciMale[1,], y1 = ciMale[2,], col = "blue")
  title(main = "Superiorities with 95% CI for males")
  
  ciFemale <- sapply(superioritiesFemale, FUN = function(x) x$conf.int)
  plot(sapply(superioritiesFemale, FUN = function(x) x$estimate), 
       x = PQBcutoffs, type = "b", ylim = c( .5, 1 ), col = "red", ylab = "")
  segments(x0 = PQBcutoffs, x1 = PQBcutoffs, 
           y0 = ciFemale[1,], y1 = ciFemale[2,], col = "red")
  title(main = "Superiorities with 95% CI for females")
  
  }
  if (save) {
  return(list(superiorities = superiorities[which.max(sapply(superiorities, 
                                                             FUN = function(x) {
                                                               x$estimate
                                                               }))], 
              superioritiesMale = superioritiesMale[which.max(sapply(superioritiesMale, 
                                                                     FUN = function(x) {
                                                                       x$estimate
                                                                     }))], 
              superioritiesFemale = superioritiesFemale[which.max(sapply(superioritiesFemale, 
                                                                         FUN = function(x) {
                                                                           x$estimate
                                                                         }))]))
  }
  print(which.max(sapply(superiorities, 
                         FUN = function(x) {
                           x$estimate
                         })))
  print(which.max(sapply(superioritiesMale, 
                         FUN = function(x) {
                           x$estimate
                         })))
  print(which.max(sapply(superioritiesFemale, 
                         FUN = function(x) {
                           x$estimate
                         })))
  
  }


# Save 'best' results 
superiority_res <- superiority_plots(PQBcutoffs,df$BDIsum,save = T, plots = F)
superiority_res[[1]][[1]]$parameter

# Plot results.

superiority_plots(PQBcutoffs, df$BDIsum)



