library(expm)

P <- matrix(c(0.6, 0.0, 0.4,
              0.7, 0.2, 0.1,
              0.0, 0.2, 0.8),
            nrow = 3, byrow = TRUE, dimnames = list(0:2, 0:2))

# (i) Balance equations: (P^T - I) pi = 0, last equation replaced by sum(pi) = 1
A <- t(P) - diag(3)
A[3, ] <- 1
pi_balance <- solve(A, c(0, 0, 1))

# (ii) Matrix power: first row of P^n, and check that all rows of P^50 are equal
n_vals    <- 1:30
conv      <- t(sapply(n_vals, function(n) (P %^% n)[1, ]))
pi_matrix <- (P %^% 50)[1, ]
stopifnot(max(abs((P %^% 50) - matrix(pi_matrix, 3, 3, byrow = TRUE))) < 1e-8)

# (iii) Simulation: fraction of time in each state, R independent runs
sim_dtmc <- function(P, steps, warmup) {
  x <- integer(steps); x[1] <- 0L                       # start in state 0
  for (t in 2:steps) x[t] <- sample(0:2, 1, prob = P[x[t - 1] + 1, ])
  tabulate(x[-(1:warmup)] + 1L, nbins = 3) / (steps - warmup)
}
set.seed(1)
R   <- 30
est <- t(replicate(R, sim_dtmc(P, 1e5, 1e3)))           # R x 3 matrix
pi_sim <- colMeans(est)
hw     <- qt(0.975, R - 1) * apply(est, 2, sd) / sqrt(R)

# Convergence figure
jpeg("E4_convergence.jpeg", width = 900, height = 550, res = 120)
matplot(n_vals, conv, type = "l", lty = 1, lwd = 2, col = 1:3,
        xlab = "n (steps)", ylab = expression(pi[i]^{(n)}))
abline(h = pi_balance, lty = 3, col = "gray50")
legend("right", legend = paste("state", 0:2), col = 1:3, lty = 1, lwd = 2)
dev.off()

results <- data.frame(state = 0:2, balance = pi_balance, matrix_n50 = pi_matrix,
                      sim = pi_sim, hw = hw, inside = abs(pi_sim - pi_balance) <= hw)
print(round(results, 4), row.names = FALSE)