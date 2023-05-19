# Regressions

# Note: Male is reference: contrasts(df$sex)
linear_model <- lm(data = df,
   formula = BDIsum ~ PQBsum_cust * sex)
summary(linear_model)


# Plot
pdf("LinearityAssumption.pdf", fonts = "serif", compress = F, pointsize = 8)
linRegPlot <- ggplot(data = df, 
       aes(x = PQBsum_cust, y = BDIsum, lty = sex, col = sex)) + 
  geom_jitter(inherit.aes = T, alpha = .4) + 
  geom_smooth(method = "lm", se = T, alpha = .1) + 
  ggtitle("Linear regressions by sex and interaction.")
dev.off()

# Residual plot

plot(y = resid(linear_model) , fitted( linear_model ) )



