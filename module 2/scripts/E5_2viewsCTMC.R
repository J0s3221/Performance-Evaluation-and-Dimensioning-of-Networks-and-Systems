# CTMC of Figure 2: rates l12 = 2, l21 = l23 = l31 = l32 = 1 (generator matrix Q)
Q <- matrix(c(-2,  2,  0,
               1, -2,  1,
               1,  1, -2), nrow = 3, byrow = TRUE)

# Theory: solve pi Q = 0 with sum(pi) = 1 (last equation replaced)
A <- t(Q); A[3, ] <- 1
pi_theory <- solve(A, c(0, 0, 1))

# View 1: hold in state i for Exp(q_i), then jump to j with probability q_ij / q_i
sim_view1 <- function(Q, n_jumps, warmup = 0, start = 1) {
  m <- nrow(Q); q <- -diag(Q)
  state <- start; clock <- 0
  time_in <- numeric(m); jump_time <- numeric(n_jumps); jump_state <- integer(n_jumps)
  for (k in 1:n_jumps) {
    jump_time[k] <- clock; jump_state[k] <- state
    h <- rexp(1, rate = q[state])                       # holding time
    if (k > warmup) time_in[state] <- time_in[state] + h
    clock <- clock + h
    p <- Q[state, ]; p[state] <- 0                      # jump probs (sample() normalizes)
    state <- sample(1:m, 1, prob = p)
  }
  list(pi = time_in / sum(time_in), t = jump_time, s = jump_state)
}

# View 2: one Exp(q_ij) timer per possible transition; the smallest timer wins
sim_view2 <- function(Q, n_jumps, warmup = 0, start = 1) {
  m <- nrow(Q)
  state <- start; clock <- 0
  time_in <- numeric(m); jump_time <- numeric(n_jumps); jump_state <- integer(n_jumps)
  for (k in 1:n_jumps) {
    jump_time[k] <- clock; jump_state[k] <- state
    rates <- Q[state, ]; rates[state] <- 0
    timers <- rep(Inf, m); pos <- rates > 0
    timers[pos] <- rexp(sum(pos), rate = rates[pos])    # competing timers
    nxt <- which.min(timers); h <- timers[nxt]
    if (k > warmup) time_in[state] <- time_in[state] + h
    clock <- clock + h
    state <- nxt
  }
  list(pi = time_in / sum(time_in), t = jump_time, s = jump_state)
}

# R independent runs per view, 95% Student-t confidence intervals
set.seed(1)
R <- 30; n_jumps <- 1e5; warmup <- 1e3
est1 <- t(replicate(R, sim_view1(Q, n_jumps, warmup)$pi))
est2 <- t(replicate(R, sim_view2(Q, n_jumps, warmup)$pi))
hw <- function(est) qt(0.975, R - 1) * apply(est, 2, sd) / sqrt(R)

results <- data.frame(state = 1:3, theory = pi_theory,
                      view1 = colMeans(est1), hw1 = hw(est1),
                      view2 = colMeans(est2), hw2 = hw(est2))
results$inside1 <- abs(results$view1 - results$theory) <= results$hw1
results$inside2 <- abs(results$view2 - results$theory) <= results$hw2
print(results, digits = 4, row.names = FALSE)

# Figure: sample paths of both views (first 40 jumps)
plot_path <- function(p, title) {
  plot(p$t, p$s, type = "s", lwd = 2, yaxt = "n", xlab = "time", ylab = "state", main = title)
  axis(2, at = 1:3)
}

jpeg("E5_paths.jpeg", width = 1000, height = 650, res = 120)
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot_path(sim_view1(Q, 40), "View 1: holding time, then jump")
plot_path(sim_view2(Q, 40), "View 2: competing timers")
dev.off()