library(expm)

P <- matrix(c(0.6, 0.0, 0.4,
              0.7, 0.2, 0.1,
              0.0, 0.2, 0.8),
            nrow = 3, byrow = TRUE,
            dimnames = list(0:2, 0:2))

# (i) Balance equations
A <- t(P) - diag(3)
A[3, ] <- 1
pi_balance <- solve(A, c(0, 0, 1))

# (ii) Matrix power
pi_matrix <- (P %^% 50)[1, ]

# (iii) Simulation
set.seed(1)
N <- 1e6
x <- numeric(N)
x[1] <- 0
for (t in 2:N) {
  x[t] <- sample(0:2, 1, prob = P[x[t-1] + 1, ])
}
pi_sim <- as.numeric(table(factor(x, levels = 0:2)) / N)

# Results table
results <- rbind("Balance equations" = pi_balance,
                 "Matrix power (n=50)" = pi_matrix,
                 "Simulation (N=1e6)" = pi_sim)
colnames(results) <- c("pi_0", "pi_1", "pi_2")

cat("\nLimiting state probabilities\n\n")
print(round(results, 4))

cat("\nMax absolute difference vs. balance equations:\n")
print(round(apply(abs(results[-1, ] - rep(pi_balance, each = 2)), 1, max), 5))