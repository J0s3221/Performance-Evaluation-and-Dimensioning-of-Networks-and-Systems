# All systems: E[S] = 2, rho = 0.5  =>  lambda = 0.25
ES <- 2; lambda <- 0.5 / ES
n_comp <- 1e5; R <- 100                       # completions per run, replications

draw_exp  <- function() rexp(1, rate = 1 / ES)                  # M/M/1
draw_bim1 <- function() ifelse(runif(1) < 0.90, 1, 11)          # mice 1, elephants 11
draw_bim2 <- function() ifelse(runif(1) < 0.99, 1, 101)         # mice 1, elephants 101

# FIFO M/G/1: average queuing delay of the first n customers served
sim_fifo <- function(draw_size, lambda, n) {
  time <- 0; n_done <- 0; busy <- FALSE; acum <- 0
  qa <- numeric(2 * n); qs <- numeric(2 * n); head <- 1; tail <- 0   # arrival times, sizes
  events <- c(rexp(1, lambda), Inf)                                  # (arrival, departure)
  while (n_done < n) {
    type <- which.min(events); time <- events[type]
    if (type == 1) {                                  # arrival
      events[1] <- time + rexp(1, lambda)
      size <- draw_size()
      if (busy) { tail <- tail + 1; qa[tail] <- time; qs[tail] <- size }
      else      { busy <- TRUE; n_done <- n_done + 1; events[2] <- time + size }
    } else if (head > tail) {                         # departure, empty queue
      busy <- FALSE; events[2] <- Inf
    } else {                                          # departure, next job starts
      acum <- acum + time - qa[head]; events[2] <- time + qs[head]
      head <- head + 1; n_done <- n_done + 1
    }
  }
  acum / n_done
}

# Same system, but mice (size = mouse_size) have non-preemptive priority over elephants
sim_prio <- function(draw_size, mouse_size, lambda, n) {
  time <- 0; n_done <- 0; busy <- FALSE; acum <- 0
  qa <- matrix(0, 2 * n, 2); qs <- matrix(0, 2 * n, 2)    # column 1 = mice, 2 = elephants
  head <- c(1, 1); tail <- c(0, 0)
  events <- c(rexp(1, lambda), Inf)
  while (n_done < n) {
    type <- which.min(events); time <- events[type]
    if (type == 1) {                                  # arrival
      events[1] <- time + rexp(1, lambda)
      size <- draw_size(); k <- if (size == mouse_size) 1 else 2
      if (busy) { tail[k] <- tail[k] + 1; qa[tail[k], k] <- time; qs[tail[k], k] <- size }
      else      { busy <- TRUE; n_done <- n_done + 1; events[2] <- time + size }
    } else {                                          # departure
      k <- which(head <= tail)[1]                     # highest-priority non-empty queue
      if (is.na(k)) { busy <- FALSE; events[2] <- Inf }
      else {
        acum <- acum + time - qa[head[k], k]; events[2] <- time + qs[head[k], k]
        head[k] <- head[k] + 1; n_done <- n_done + 1
      }
    }
  }
  acum / n_done
}

# Theory: Pollaczek-Khinchine (FIFO) and Cobham (priority to mice)
theory_fifo <- function(s1, p1, s2, p2) lambda * (s1^2*p1 + s2^2*p2) / (2 * (1 - 0.5))
theory_prio <- function(s1, p1, s2, p2) {
  W0 <- lambda * (s1^2*p1 + s2^2*p2) / 2; rho_m <- lambda * p1 * s1
  p1 * W0 / (1 - rho_m) + p2 * W0 / ((1 - rho_m) * (1 - 0.5))
}
theory <- c(ES * 0.5 / (1 - 0.5),                      # M/M/1: rho*ES/(1-rho) = 2
            theory_fifo(1, .9, 11, .1),   theory_prio(1, .9, 11, .1),
            theory_fifo(1, .99, 101, .01), theory_prio(1, .99, 101, .01))

cases <- list("M/M/1"   = function() sim_fifo(draw_exp,  lambda, n_comp),
              "B1 FIFO" = function() sim_fifo(draw_bim1, lambda, n_comp),
              "B1 prio" = function() sim_prio(draw_bim1, 1, lambda, n_comp),
              "B2 FIFO" = function() sim_fifo(draw_bim2, lambda, n_comp),
              "B2 prio" = function() sim_prio(draw_bim2, 1, lambda, n_comp))

set.seed(1)
samples <- sapply(cases, function(f) replicate(R, f()))        # R x 5 matrix

res <- data.frame(case = names(cases), theory = theory, mean = colMeans(samples),
                  hw  = qt(0.975, R - 1) * apply(samples, 2, sd) / sqrt(R),
                  var = apply(samples, 2, var))
res$rel_err_pct <- 100 * (res$mean - res$theory) / res$theory
write.csv(res, "E8_results.csv", row.names = FALSE)
print(res, digits = 4, row.names = FALSE)

# Figure: distribution of the 100 estimates per case (red diamonds = theory)
jpeg("E8_boxplot.jpeg", width = 1000, height = 600, res = 120)
boxplot(samples, log = "y", las = 1, ylab = "Average queuing delay (log scale)")
points(1:5, theory, pch = 18, col = "red", cex = 1.8)
legend("topleft", legend = "Theory", pch = 18, col = "red")
dev.off()