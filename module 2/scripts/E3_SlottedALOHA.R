source("~/DDRS/Performance-Evaluation-and-Dimensioning-of-Networks-and-Systems/module 2/scripts_helper/aloha/aloha_theo.R")   # adjust the path if the file is elsewhere


# One replication. State = number of BACKLOGGED users (n).
simulate_aloha <- function(N, sigma, p, warmup, measured) {
  n <- 0L; successes <- 0L
  for (slot in 1:(warmup + measured)) {
    t <- rbinom(1, N - n, sigma)   # THINKING users that transmit
    b <- rbinom(1, n, p)           # BACKLOGGED users that transmit
    if (t + b == 1L) {             # success
      if (slot > warmup) successes <- successes + 1L
      if (b == 1L) n <- n - 1L     # a backlogged user succeeded
    } else {                       # idle or collision
      n <- n + t                   # colliding THINKING users become BACKLOGGED
    }
  }
  successes / measured
}

set.seed(5003)
scen <- data.frame(N = 10, sigma = c(0.001, 0.05, 0.1, 0.1), p = c(0.3, 0.3, 0.3, 0.6))
R <- 20; warmup <- 1e4; measured <- 2e5

res <- do.call(rbind, lapply(1:nrow(scen), function(i) with(scen[i, ], {
  x  <- replicate(R, simulate_aloha(N, sigma, p, warmup, measured))
  hw <- qt(0.975, R - 1) * sd(x) / sqrt(R)          # 95% CI half-width
  th <- aloha_theo(N = N, p = p, s = sigma)
  data.frame(N, sigma, p, theory = th, sim = mean(x), hw = hw,
             inside = abs(mean(x) - th) <= hw, rel_err_pct = 100 * (mean(x) - th) / th)
})))
write.csv(res, "E3_ALOHA_comparison.csv", row.names = FALSE)
print(res, row.names = FALSE)