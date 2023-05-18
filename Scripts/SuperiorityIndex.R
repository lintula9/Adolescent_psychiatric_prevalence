# Probability of superiority

# BDI Mild -------
PQBcutoffs <- 1:40

superiority_plots <- function(PQBcutoffs, BDI) {
  # Both sexes.
  superiorities <- lapply(PQBcutoffs, FUN = function (x){
    brunnermunzel.test(table(BDI,df$PQBsum_cust >= x))
    
  })

  # Male.
  superioritiesMale <- lapply(PQBcutoffs, FUN = function (x){
    brunnermunzel.test(table(BDI[ df$sex == "Male" ],
                             df$PQBsum_cust[ df$sex == "Male" ] >= x))})

  
  # Female.
  superioritiesFemale <- lapply(PQBcutoffs, FUN = function (x){
    brunnermunzel.test(table(BDI[ df$sex == "Female" ],
                             df$PQBsum_cust[ df$sex == "Female" ] >= x))
    
  })
  plot(sapply(superioritiesFemale, FUN = function(x) x$estimate), 
       x = PQBcutoffs, type = "b", ylim = c( .5, 1 ), col = "red", ylab = "")
  lines(y = sapply(superioritiesMale, FUN = function(x) x$estimate), 
        x = PQBcutoffs, type = "b", col = "blue", lwd = 1.5)
  lines(y = sapply(superiorities, FUN = function(x) x$estimate), 
        x = PQBcutoffs, type = "b", col = "gray", lwd = 1.5)
  title(ylab = "Superiority: P( X < Y ) + .5 * P( X = Y )")
  legend(x = 35, y = .9, 
         legend = c("Overall", "Male", "Female"), 
         col = c("black","blue","red"), 
         lty = 1, pch = 1, 
         box.col = "white") 
  }



pdf("Superiority_MDD&Mild.pdf", fonts = "serif", compress = F, pointsize = 8)
# Plots
superiority_plots(PQBcutoffs,df$BDIMDD) ;  title(main = "Superiority plot for major depression")
superiority_plots(PQBcutoffs,df$BDImild) ;  title(main = "Superiority plot for mild depression")
dev.off()


