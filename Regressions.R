# Regressions

# Note: Male is reference: contrasts(df$sex)
linear_model <- lm(data = df,
   formula = BDIsum ~ PQBsum_cust * sex)
summary(linear_model)


# Plot
pdf("LinearityAssumption.pdf", fonts = "serif", compress = F, pointsize = 8)
ggplot(data = df, 
       aes(x = PQBsum_cust, y = BDIsum, col = sex)) + 
  geom_jitter(inherit.aes = T) + geom_smooth(method = "lm", se = T, alpha = .1) + 
  geom_smooth(se=F)
dev.off()

# Residual plot

plot(y = resid(linear_model) , fitted( linear_model ) )
abline(h = 0)
residualSpline <- fitted(lm(resid( linear_model ) ~ bs(fitted( linear_model )) ))
lines( x = fitted( linear_model ), y = residualSpline )


