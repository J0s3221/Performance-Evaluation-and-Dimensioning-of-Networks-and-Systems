source("~/DDRS/Performance-Evaluation-and-Dimensioning-of-Networks-and-Systems/module 2/scripts_helper/alhoa/aloha_theo.R")   # adjust the path if the file is elsewhere

simulate_aloha <- function(N, sigma, p, warmup_slots, measured_slots) {
  backlogged <- rep(FALSE, N)
  successful_transmissions <- 0L

  for (slot in seq_len(warmup_slots + measured_slots)) {
    attempt_probability <- ifelse(backlogged, p, sigma)
    transmits <- runif(N) < attempt_probability
    number_transmitting <- sum(transmits)

    if (number_transmitting == 1L) {
      if (slot > warmup_slots) successful_transmissions <- successful_transmissions + 1L
      backlogged[transmits] <- FALSE
    } else {
      backlogged[transmits & !backlogged] <- TRUE
    }
  }

  successful_transmissions / measured_slots
}

set.seed(5003)
scenarios <- data.frame(
  N     = c(10L, 10L, 10L),
  sigma = c(0.001, 0.1, 0.1),
  p     = c(0.3, 0.3, 0.6)
)
warmup_slots   <- 100000L
measured_slots <- 1000000L

scenarios$theoretical_throughput <- mapply(
  aloha_theo,
  N = scenarios$N,
  p = scenarios$p,
  s = scenarios$sigma
)
scenarios$simulated_throughput <- mapply(
  simulate_aloha,
  N = scenarios$N,
  sigma = scenarios$sigma,
  p = scenarios$p,
  MoreArgs = list(warmup_slots = warmup_slots, measured_slots = measured_slots)
)

write.csv(scenarios, "E3_ALOHA_comparison.csv", row.names = FALSE)
print(scenarios, row.names = FALSE)