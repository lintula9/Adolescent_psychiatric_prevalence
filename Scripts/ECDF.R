# Empirical CDFs.

#Distress
plot(y = 1 - ecdf(df$PQBsum_cust)(sort((unique(df$PQBsum_cust)))), 
     sort(unique(df$PQBsum_cust)),
     main = "Spline cutoffs using all data",
     ylab = "Inverse Cumulative Proportion",
     xlab = "PQ-B Distress", 
     cex.axis = 1.5,
     ylim = c(0,1),
     type = "l",
     col = cols["Total"])
lines(y = 1 - ecdf(df$PQBsum_cust[df$sex == "Male"])(sort((unique(df$PQBsum_cust)))), 
      sort(unique(df$PQBsum_cust)),
      col = cols["Male"],
      lty = 2)
lines(y = 1 - ecdf(df$PQBsum_cust[df$sex == "Female"])(sort((unique(df$PQBsum_cust)))), 
      sort(unique(df$PQBsum_cust)),
      col = cols[3],
      lty = 3)
legend(x = 70, y = .2, 
       legend = c("Overall", "Male", "Female"), 
       col = cols, 
       cex = 1.5,
       lwd = 3,
       lty = 1:3,
       box.col = "white") 

# Total
plot(y = 1 - ecdf(df$PQBdicsum)(sort((unique(df$PQBdicsum)))), 
     sort(unique(df$PQBdicsum)),
     main = "Spline cutoffs using all data",
     ylab = "Inverse Cumulative Proportion",
     xlab = "PQ-B Distress", 
     cex.axis = 1.5,
     ylim = c(0,1),
     type = "l",
     col = cols["Total"],
     xlim = c(0,19))
lines(y = 1 - ecdf(df$PQBdicsum[df$sex == "Male"])(sort((unique(df$PQBdicsum)))), 
      sort(unique(df$PQBdicsum)),
      col = cols["Male"],
      lty = 2)
lines(y = 1 - ecdf(df$PQBdicsum[df$sex == "Female"])(sort((unique(df$PQBdicsum)))), 
      sort(unique(df$PQBdicsum)),
      col = cols[3],
      lty = 3)
legend(x = 70, y = .2, 
       legend = c("Overall", "Male", "Female"), 
       col = cols, 
       cex = 1.5,
       lwd = 3,
       lty = 1:3,
       box.col = "white") 


  