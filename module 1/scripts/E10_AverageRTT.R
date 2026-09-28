# RTT samples (ms) collected via ping to www.uq.edu.au
rtt <- c(325, 317, 316, 316, 316, 317, 316, 317, 316, 321)   # RTT samples (ms)

n    <- length(rtt)
xbar <- mean(rtt)
s    <- sd(rtt)
se   <- s / sqrt(n)          # standard error of the mean

alpha <- 0.05                 # for a 95% CI
df    <- n - 1

# --- Critical points ---
z_crit <- qnorm(1 - alpha/2)          # Normal distribution
t_crit <- qt(1 - alpha/2, df = df)    # Student's t distribution

# --- Confidence intervals ---
ci_normal <- c(xbar - z_crit * se, xbar + z_crit * se)
ci_t      <- c(xbar - t_crit * se, xbar + t_crit * se)

cat("n =", n, "\n")
cat("Sample mean (ms):", round(xbar, 3), "\n")
cat("Sample sd (ms):  ", round(s, 3), "\n")
cat("Standard error:  ", round(se, 3), "\n\n")

cat("z critical value (Normal):", round(z_crit, 4), "\n")
cat("t critical value (Student's t, df =", df, "):", round(t_crit, 4), "\n\n")

cat("95% CI (Normal):     [", round(ci_normal[1], 3), ",", round(ci_normal[2], 3), "] ms\n")
cat("95% CI (Student's t):[", round(ci_t[1], 3), ",", round(ci_t[2], 3), "] ms\n")

pdf("E10_CI.pdf", width = 7, height = 3.2)
plot(NA, xlim = range(c(rtt, ci_normal, ci_t)) + c(-2, 2), ylim = c(0.5, 3.5),
     yaxt = "n", xlab = "RTT (ms)", ylab = "")
axis(2, at = c(3, 2, 1), labels = c("Samples", "Normal CI", "Student t CI"), las = 1)
points(rtt, rep(3, n), pch = 16, col = "gray40")
segments(ci_normal[1], 2, ci_normal[2], 2, lwd = 3, col = "blue")
segments(ci_t[1], 1, ci_t[2], 1, lwd = 3, col = "red")
abline(v = xbar, lty =   2)
dev.off()