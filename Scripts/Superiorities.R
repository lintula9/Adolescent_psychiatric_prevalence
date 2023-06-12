# Probability of superiority



# BDI Mild -------
PQBcuts <- 1:40

# Function that plots, and saves results if necessary.
superiority_plots <- function(PQBcutoffs = PQBcuts, BDI = df$BDIsum, save = F, plots = T, sexstrat = T, ciplot = T) {
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
       x = PQBcutoffs, type = "b", ylim = c( .5, 1 ), 
       col = cols["Female"], 
       lty = 3, lwd = 1.5, pch = 2,
       ylab = "", xlab = "")
  lines(y = sapply(superioritiesMale, FUN = function(x) x$estimate), 
        x = PQBcutoffs, type = "b", 
        col = cols["Male"], 
        lty = 2, lwd = 1.5, pch = 0)
  lines(y = sapply(superiorities, FUN = function(x) x$estimate), 
        x = PQBcutoffs, type = "b", 
        col = cols["Total"], 
        lty = 1, lwd = 1.5, pch = 1)
  title(ylab = "Superiority index", 
        xlab = "PQB cut-off", 
        cex.lab = 1.5 )
  legend(x = 35, y = .6, 
         legend = c("Overall", "Male", "Female"), 
         col = cols, 
         lty = 1:3, 
         cex = 1.5,
         pch = c(1, 0, 2), 
         box.col = "white")
  title(main = "") }
  if( ciplot ){
    
  ciAll <- sapply(superiorities, FUN = function(x) x$conf.int)
  plot(sapply(superiorities, FUN = function(x) x$estimate), 
       x = PQBcutoffs, type = "b", ylim = c( .5, 1 ), col = cols["Total"], 
       ylab = "", xlab = "")
  segments(x0 = PQBcutoffs, x1 = PQBcutoffs, 
           y0 = ciAll[1,], y1 = ciAll[2,], col = cols["Total"] )
  title(ylab = "Superiority index", 
        xlab = "PQB cut-off", main = "Total",
        cex.lab = 1.5 )
  }
  if( sexstrat ) {
    
  ciMale <- sapply(superioritiesMale, FUN = function(x) x$conf.int)
  plot(sapply(superioritiesMale, FUN = function(x) x$estimate), 
       x = PQBcutoffs, type = "b", ylim = c( .5, 1 ), col = cols["Male"], 
       ylab = "", xlab = "")
  segments(x0 = PQBcutoffs, x1 = PQBcutoffs, 
           y0 = ciMale[1,], y1 = ciMale[2,], col = cols["Male"])
  title(ylab = "Superiority index", 
        xlab = "PQB cut-off", main = "Males",
        cex.lab = 1.5 )
  
  ciFemale <- sapply(superioritiesFemale, FUN = function(x) x$conf.int)
  plot(sapply(superioritiesFemale, FUN = function(x) x$estimate), 
       x = PQBcutoffs, type = "b", ylim = c( .5, 1 ), col = cols["Female"], 
       ylab = "", xlab = "")
  segments(x0 = PQBcutoffs, x1 = PQBcutoffs, 
           y0 = ciFemale[1,], y1 = ciFemale[2,], col = cols["Female"])
  title(ylab = "Superiority index", 
        xlab = "PQB cut-off", main = "Females",
        cex.lab = 1.5 )
  
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
superiority_res <- superiority_plots(PQBcuts,df$BDIsum,
                                     save = T, 
                                     plots = F)

