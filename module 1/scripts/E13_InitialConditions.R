# Exercise 13: effect of initial conditions on M/M/1 queue-delay estimates.

arrival_rate <- 3
service_rate <- 4
run_lengths <- c(20, 2000)
initial_clients_cases <- c(0, 10)
number_of_runs <- 25
random_seed <- 13

simulate_mm1_average_queue_delay <- function(
  arrival_rate,
  service_rate,
  number_of_delays,
  initial_clients
) {
  time <- 0
  completed <- 0
  accumulated_queue_delay <- 0
  queue_arrival_times <- if (initial_clients > 1) {
    rep(0, initial_clients - 1)
  } else {
    numeric(0)
  }
  queue_head <- 1
  server_busy <- initial_clients > 0
  current_customer_queue_delay <- 0
  next_arrival <- rexp(1, rate = arrival_rate)
  next_departure <- if (server_busy) rexp(1, rate = service_rate) else Inf

  while (completed < number_of_delays) {
    if (next_arrival < next_departure) {
      time <- next_arrival
      next_arrival <- time + rexp(1, rate = arrival_rate)

      if (server_busy) {
        queue_arrival_times <- c(queue_arrival_times, time)
      } else {
        server_busy <- TRUE
        current_customer_queue_delay <- 0
        next_departure <- time + rexp(1, rate = service_rate)
      }
    } else {
      time <- next_departure
      accumulated_queue_delay <- accumulated_queue_delay + current_customer_queue_delay
      completed <- completed + 1

      if (queue_head <= length(queue_arrival_times)) {
        current_customer_queue_delay <- time - queue_arrival_times[queue_head]
        queue_head <- queue_head + 1
        next_departure <- time + rexp(1, rate = service_rate)
      } else {
        server_busy <- FALSE
        next_departure <- Inf
      }
    }
  }

  accumulated_queue_delay / number_of_delays
}

set.seed(random_seed)
run_results <- expand.grid(
  run = seq_len(number_of_runs),
  run_length = run_lengths,
  initial_clients = initial_clients_cases,
  KEEP.OUT.ATTRS = FALSE
)
run_results$average_queue_delay <- mapply(
  simulate_mm1_average_queue_delay,
  number_of_delays = run_results$run_length,
  initial_clients = run_results$initial_clients,
  MoreArgs = list(arrival_rate = arrival_rate, service_rate = service_rate)
)
run_results$scenario <- factor(
  paste0("n = ", run_results$run_length, ", initial = ", run_results$initial_clients),
  levels = c(
    "n = 20, initial = 0",
    "n = 20, initial = 10",
    "n = 2000, initial = 0",
    "n = 2000, initial = 10"
  )
)

traffic_intensity <- arrival_rate / service_rate
theoretical_queue_delay <- traffic_intensity / (service_rate - arrival_rate)
scenario_groups <- split(run_results$average_queue_delay, run_results$scenario)
summary_results <- do.call(rbind, lapply(names(scenario_groups), function(scenario_name) {
  estimates <- scenario_groups[[scenario_name]]
  estimate_mean <- mean(estimates)
  estimate_sd <- sd(estimates)
  confidence_interval <- estimate_mean + c(-1, 1) *
    qt(0.975, df = length(estimates) - 1) * estimate_sd / sqrt(length(estimates))

  data.frame(
    scenario = scenario_name,
    mean = estimate_mean,
    standard_deviation = estimate_sd,
    ci_lower = confidence_interval[1],
    ci_upper = confidence_interval[2],
    minimum = min(estimates),
    maximum = max(estimates),
    stringsAsFactors = FALSE
  )
}))

cat("M/M/1 initial-condition experiment\n")
cat("Arrival rate (lambda):", arrival_rate, "\n")
cat("Service rate (mu):", service_rate, "\n")
cat("Traffic intensity (lambda/mu):", traffic_intensity, "\n")
cat("Initial condition: with 10 clients, one starts in service and nine wait from t = 0.\n")
cat("Replications per case:", number_of_runs, "\n")
cat("Theoretical average queue delay:", round(theoretical_queue_delay, 4), "\n\n")
print(transform(
  summary_results,
  mean = round(mean, 4),
  standard_deviation = round(standard_deviation, 4),
  ci_lower = round(ci_lower, 4),
  ci_upper = round(ci_upper, 4),
  minimum = round(minimum, 4),
  maximum = round(maximum, 4)
), row.names = FALSE)

script_path <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
if (!is.null(script_path) && nzchar(script_path)) {
  script_dir <- dirname(normalizePath(script_path, winslash = "/"))
} else {
  search_roots <- unique(c(
    normalizePath(getwd(), winslash = "/"),
    list.dirs(getwd(), recursive = FALSE, full.names = TRUE),
    dirname(normalizePath(getwd(), winslash = "/"))
  ))
  possible_output_dirs <- c(
    file.path(search_roots, "module 1", "scripts"),
    file.path(search_roots, "scripts")
  )
  existing_output_dirs <- possible_output_dirs[dir.exists(possible_output_dirs)]
  script_dir <- if (length(existing_output_dirs)) existing_output_dirs[1] else getwd()
}

write.csv(run_results, file.path(script_dir, "E13_replication_results.csv"), row.names = FALSE)
write.csv(summary_results, file.path(script_dir, "E13_summary_results.csv"), row.names = FALSE)

pdf(file.path(script_dir, "E13_initial_conditions.pdf"), width = 9, height = 5.5)
par(mar = c(7, 5, 3, 1))
boxplot(
  average_queue_delay ~ scenario,
  data = run_results,
  las = 2,
  ylab = "Average delay in queue",
  xlab = "Run length and initial number of clients",
  main = "Effect of initial conditions on M/M/1 estimates",
  col = c("lightblue", "mistyrose", "lightblue", "mistyrose")
)
abline(h = theoretical_queue_delay, col = "red", lty = 2, lwd = 2)
legend(
  "topright",
  legend = "Theoretical value (0.75)",
  col = "red",
  lty = 2,
  lwd = 2,
  bty = "n"
)
dev.off()

cat("\nRun-level data:", file.path(script_dir, "E13_replication_results.csv"), "\n")
cat("Summary data:", file.path(script_dir, "E13_summary_results.csv"), "\n")
cat("Plot:", file.path(script_dir, "E13_initial_conditions.pdf"), "\n")
