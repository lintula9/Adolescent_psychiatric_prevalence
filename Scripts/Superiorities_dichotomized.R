# CutOffs for dichotomized BDI -----

PQBcutoffs <- 1:40 # Set cut-offs.

# Mild depression ------
mildCuts <- function() { lapply( PQBcutoffs, # Using all cases.
       FUN = function (x){
            brunnermunzel.test(x = df$BDImild[df$PQBsum_cust < x],
                               y = df$BDImild[df$PQBsum_cust >= x])
 })}


      # #
mildCutsMales <- function() { lapply(PQBcutoffs, # Males only.
       FUN = function (x){
            maledat <- df[df$sex == "Male" , ]
            brunnermunzel.test(x = df$BDImild[ df$sex == "Male" ] [maledat$PQBsum_cust < x ],
                               y = df$BDImild[ df$sex == "Male" ] [maledat$PQBsum_cust >= x ])
 })}


      # #
mildCutsFemales <- function() { lapply(PQBcutoffs, # Females ony.
       FUN = function (x){
            femaledat <- df[ df$sex == "Female" , ]
            brunnermunzel.test(x = df$BDImild[ df$sex == "Female" ][ femaledat$PQBsum_cust < x ],
                               y = df$BDImild[ df$sex == "Female" ][ femaledat$PQBsum_cust >= x ])
            
 })}

# Major depression (MDD) ------
MDDCuts <- function() { lapply( PQBcutoffs, # Using all cases.
                                 FUN = function (x){
                                   brunnermunzel.test(x = df$BDIMDD[df$PQBsum_cust < x],
                                                      y = df$BDIMDD[df$PQBsum_cust >= x])
                                 })}


# #
MDDCutsMales <- function() { lapply(PQBcutoffs, # Males only.
                                     FUN = function (x){
                                       maledat <- df[df$sex == "Male" , ]
                                       brunnermunzel.test(x = df$BDIMDD[ df$sex == "Male" ] [maledat$PQBsum_cust < x ],
                                                          y = df$BDIMDD[ df$sex == "Male" ] [maledat$PQBsum_cust >= x ])
                                     })}


# #
MDDCutsFemales <- function() { lapply(PQBcutoffs, # Females ony.
                                       FUN = function (x){
                                         femaledat <- df[ df$sex == "Female" , ]
                                         brunnermunzel.test(x = df$BDIMDD[ df$sex == "Female" ][ femaledat$PQBsum_cust < x ],
                                                            y = df$BDIMDD[ df$sex == "Female" ][ femaledat$PQBsum_cust >= x ])
                                         
                                       })}

# Find best cut-offs -------

which.max(sapply(mildCuts(), FUN = function(x) x$estimate))
which.max(sapply(mildCutsMales(), FUN = function(x) x$estimate))
which.max(sapply(mildCutsFemales(), FUN = function(x) x$estimate))
which.max(sapply(MDDCuts(), FUN = function(x) x$estimate))
which.max(sapply(MDDCutsMales(), FUN = function(x) x$estimate))
which.max(sapply(MDDCutsFemales(), FUN = function(x) x$estimate))

plot(sapply(mildCuts(), FUN = function(x) x$p.value), col = cols["Total"])
plot(sapply(MDDCuts(), FUN = function(x) x$p.value), col = cols["Total"])
plot(sapply(mildCutsMales(), FUN = function(x) x$p.value), col = cols["Male"])
plot(sapply(MDDCutsMales(), FUN = function(x) x$p.value), col = cols["Male"])
plot(sapply(mildCutsFemales(), FUN = function(x) x$p.value), col = cols["Female"])
plot(sapply(MDDCutsFemales(), FUN = function(x) x$p.value), col = cols["Female"])
# Plotting function --------

superiority_plots_mildDepression <- function(){ # Mild
        plot(sapply(mildCutsFemales(), FUN = function(x) x$estimate), 
             x = PQBcutoffs, 
             type = "b", 
             ylim = c( .5, 1 ), 
             col = cols["Female"], 
             lty = 3, lwd = 1.5, pch = 2,
             ylab = "", xlab = "")
        lines(y = sapply(mildCutsMales(), FUN = function(x) x$estimate), 
              x = PQBcutoffs, 
              type = "b", 
              col = cols["Male"], 
              lty = 2, lwd = 1.5, pch = 0)
        lines(y = sapply(mildCuts(), FUN = function(x) x$estimate), 
              x = PQBcutoffs, 
              type = "b", 
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
               box.col = "white", bty = "n")
        title(main = "")}

superiority_plots_MDD <- function(){ # MDD
  plot(sapply(MDDCutsFemales(), FUN = function(x) x$estimate), 
       x = PQBcutoffs, 
       type = "b", 
       ylim = c( .5, 1 ), 
       col = cols["Female"], 
       lty = 3, lwd = 1.5, pch = 2,
       ylab = "", xlab = "")
  lines(y = sapply(MDDCutsMales(), FUN = function(x) x$estimate), 
        x = PQBcutoffs, 
        type = "b", 
        col = cols["Male"], 
        lty = 2, lwd = 1.5, pch = 0)
  lines(y = sapply(MDDCuts(), FUN = function(x) x$estimate), 
        x = PQBcutoffs, 
        type = "b", 
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
  title(main = "")}


