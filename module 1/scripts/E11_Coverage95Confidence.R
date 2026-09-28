# Estimates the coverage of 95% confidence intervals (based on Student's t)
# for the mean, for several distributions and sample sizes

E <- 1000                              # number of experiments
SizesOfSamples <- c(5, 10, 50, 5000)   # sizes of samples
tcritical <- qt(0.975, df = SizesOfSamples - 1)   # critical points, t distribution

# Estimates the CI coverage for one distribution
#   rgen: function(N) that generates a sample of size N
#   mu:   true (population) mean
coverage <- function(rgen, mu) {
  InInt <- rep(0, length(SizesOfSamples))
  for (i in seq_along(SizesOfSamples)) {
    N <- SizesOfSamples[i]
    for (j in 1:E) {
      a  <- rgen(N)
      ma <- mean(a)
      va <- var(a)
      alinf <- ma - tcritical[i] * sqrt(va / N)
      alsup <- ma + tcritical[i] * sqrt(va / N)
      if (mu > alinf & mu < alsup) InInt[i] <- InInt[i] + 1
    }
  }
  return(InInt / E)
}

# Standard Normal (mu = 0, sigma^2 = 1): mean = 0
cov_norm <- coverage(function(N) rnorm(N), mu = 0)

# Exponential (lambda = 1): mean = 1/lambda = 1
cov_exp <- coverage(function(N) rexp(N, rate = 1), mu = 1)

# Lognormal (mu = 0, sigma^2 = 1): mean = exp(mu + sigma^2/2)
# NOTE: rlnorm takes the standard deviation (sdlog), not the variance
cov_ln1 <- coverage(function(N) rlnorm(N, meanlog = 0, sdlog = 1), mu = exp(0 + 1/2))

# Lognormal (mu = 0, sigma^2 = 2)
cov_ln2 <- coverage(function(N) rlnorm(N, meanlog = 0, sdlog = sqrt(2)), mu = exp(0 + 2/2))

# Results table
results <- data.frame(n = SizesOfSamples,
                      Normal = cov_norm, Exponential = cov_exp,
                      LogN_s2_1 = cov_ln1, LogN_s2_2 = cov_ln2)
print(results)

# Plot: coverage vs sample size, with the nominal 95% level
pdf("E11_Coverage.pdf", width = 7, height = 4)
par(mar = c(4, 4, 1, 1))
matplot(SizesOfSamples, results[, -1], type = "b", log = "x", pch = 1:4,
        lty = 1, col = c("black", "blue", "red", "darkgreen"),
        ylim = c(0.6, 1), xlab = "Sample size n", ylab = "Estimated coverage")
abline(h = 0.95, lty = 2)
legend("bottomright", legend = c("Normal", "Exponential",
       "Lognormal (s2=1)", "Lognormal (s2=2)"),
       pch = 1:4, lty = 1, col = c("black", "blue", "red", "darkgreen"))
dev.off()