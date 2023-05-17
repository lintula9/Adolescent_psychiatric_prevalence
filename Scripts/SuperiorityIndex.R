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
        x = PQBcutoffs, type = "b", col = "blue")
  lines(y = sapply(superiorities, FUN = function(x) x$estimate), 
        x = PQBcutoffs, type = "b", col = "gray")
  title(ylab = "Superiority: P( X < Y ) + .5 * P( X = Y )")
  text(x = which.max(sapply(superiorities, FUN = function(x) x$estimate)),
       y = .75, 
       labels = paste(which.max(sapply(superiorities, FUN = function(x) x$estimate)),
                      ":",
                      round(sapply(superiorities, FUN = function(x) x$estimate)[which.max(sapply(superiorities, FUN = function(x) x$estimate))],2)))
}
pdf("Superiority_MDD&Mild.pdf", fonts = "serif", compress = F, pointsize = 8)

# Plots
superiority_plots(PQBcutoffs,df$BDIMDD)
superiority_plots(PQBcutoffs,df$BDImild)
dev.off()


