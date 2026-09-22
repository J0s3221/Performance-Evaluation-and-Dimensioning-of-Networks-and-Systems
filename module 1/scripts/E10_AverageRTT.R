# RTT samples (ms) collected via ping to www.uq.edu.au
rtt_all <- c(398, 396, 424, 396, 396, 397, 398, 396, 644, 393)

rtt <- rtt_all[-10]

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