Q <- matrix(c(-2,  2,  0,
               1, -2,  1,
               1,  1, -2),
            nrow = 3, byrow = TRUE)

A <- t(Q)
A[3, ] <- 1
pi_theory <- solve(A, c(0, 0, 1))

sim_view1 <- function(Q, n_jumps, start = 1) {
  m <- nrow(Q)
  q <- -diag(Q)
  state <- start
  time_in <- numeric(m)
  for (k in 1:n_jumps) {
    h <- rexp(1, rate = q[state])          # holding time
    time_in[state] <- time_in[state] + h
    p <- Q[state, ]; p[state] <- 0
    p <- p / q[state]                      # jump probabilities
    state <- sample(1:m, 1, prob = p)
  }
  time_in / sum(time_in)
}

sim_view2 <- function(Q, n_jumps, start = 1) {
  m <- nrow(Q)
  state <- start
  time_in <- numeric(m)
  for (k in 1:n_jumps) {
    rates <- Q[state, ]; rates[state] <- 0
    t <- rep(Inf, m)
    idx <- which(rates > 0)
    t[idx] <- rexp(length(idx), rate = rates[idx])
    nxt <- which.min(t)
    time_in[state] <- time_in[state] + t[nxt]
    state <- nxt
  }
  time_in / sum(time_in)
}

set.seed(1)
N <- 1e6
pi_v1 <- sim_view1(Q, N)
pi_v2 <- sim_view2(Q, N)

results <- rbind("Theory (solve)" = pi_theory,
                 "Simulation view 1" = pi_v1,
                 "Simulation view 2" = pi_v2)
colnames(results) <- c("pi_1", "pi_2", "pi_3")
print(round(results, 4))