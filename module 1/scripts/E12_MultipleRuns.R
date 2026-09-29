# Exercise 12: compare independent M/M/1 simulation runs.
# The arrival and service rates are chosen so that lambda / mu = 0.8.

arrival_rate <- 0.8
service_rate <- 1
completed_customers <- 1000
number_of_runs <- 10
random_seed <- 12

script_path <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
if (!is.null(script_path) && nzchar(script_path)) {
  script_dir <- dirname(normalizePath(script_path, winslash = "/"))
} else {
  roots <- unique(c(normalizePath(getwd(), winslash = "/"), dirname(normalizePath(getwd(), winslash = "/"))))
  script_candidates <- c(file.path(roots, "module 1", "scripts"), file.path(roots, "scripts"))
  script_candidates <- script_candidates[dir.exists(script_candidates)]
  if (!length(script_candidates)) stop("Could not locate the module 1/scripts directory.")
  script_dir <- script_candidates[1]
}

# Parameterized equivalent of the event-driven algorithm in scripts_helper/mm1.R.
simulate_mm1_queue_delay <- function(arrival_rate, service_rate, completed_customers) {
  time <- 0
  completed <- 0
  server_busy <- FALSE
  queue_arrival_times <- numeric(0)
  queue_head <- 1
  queue_tail <- 0
  accumulated_queue_delay <- 0
  event_times <- c(rexp(1, rate = arrival_rate), Inf)

  while (completed < completed_customers) {
    next_event <- which.min(event_times)
    time <- event_times[next_event]

    if (next_event == 1) {
      event_times[1] <- time + rexp(1, rate = arrival_rate)
      if (server_busy) {
        queue_tail <- queue_tail + 1
        if (queue_tail > length(queue_arrival_times)) {
          length(queue_arrival_times) <- max(1024, 2 * length(queue_arrival_times))
        }
        queue_arrival_times[queue_tail] <- time
      } else {
        server_busy <- TRUE
        event_times[2] <- time + rexp(1, rate = service_rate)
        completed <- completed + 1
      }
    } else if (queue_head <= queue_tail) {
      accumulated_queue_delay <- accumulated_queue_delay + time - queue_arrival_times[queue_head]
      queue_head <- queue_head + 1
      event_times[2] <- time + rexp(1, rate = service_rate)
      completed <- completed + 1
    } else {
      server_busy <- FALSE
      event_times[2] <- Inf
    }
  }

  accumulated_queue_delay / completed_customers
}

set.seed(random_seed)
queue_delay_estimates <- replicate(
  number_of_runs,
  simulate_mm1_queue_delay(arrival_rate, service_rate, completed_customers)
)

traffic_intensity <- arrival_rate / service_rate
theoretical_queue_delay <- traffic_intensity / (service_rate - arrival_rate)
replication_mean <- mean(queue_delay_estimates)
replication_sd <- sd(queue_delay_estimates)
replication_ci <- replication_mean + c(-1, 1) * qt(0.975, df = number_of_runs - 1) *
  replication_sd / sqrt(number_of_runs)

run_results <- data.frame(
  run = seq_len(number_of_runs),
  average_queue_delay = queue_delay_estimates
)

cat("M/M/1 multiple-run experiment\n")
cat("Arrival rate (lambda):", arrival_rate, "\n")
cat("Service rate (mu):", service_rate, "\n")
cat("Traffic intensity (lambda/mu):", traffic_intensity, "\n")
cat("Completed customers per run:", completed_customers, "\n")
cat("Number of independent runs:", number_of_runs, "\n\n")
print(transform(run_results, average_queue_delay = round(average_queue_delay, 4)), row.names = FALSE)
cat("\nMean across runs:", round(replication_mean, 4), "\n")
cat("Standard deviation across runs:", round(replication_sd, 4), "\n")
cat("Minimum across runs:", round(min(queue_delay_estimates), 4), "\n")
cat("Maximum across runs:", round(max(queue_delay_estimates), 4), "\n")
cat("95% Student's t CI for the mean across runs: [",
  round(replication_ci[1], 4), ", ", round(replication_ci[2], 4), "]\n", sep = "")
cat("Theoretical M/M/1 average queue delay:", round(theoretical_queue_delay, 4), "\n")

pdf(file.path(script_dir, "E12_delay_runs.pdf"), width = 8, height = 5)
par(mar = c(5, 5, 3, 1))
plot(
  run_results$run,
  run_results$average_queue_delay,
  pch = 19,
  xaxt = "n",
  xlab = "Simulation run",
  ylab = "Average delay in queue",
  main = "M/M/1 average queue delay across 10 runs",
  ylim = range(c(queue_delay_estimates, theoretical_queue_delay))
)
axis(1, at = run_results$run)
abline(h = theoretical_queue_delay, col = "red", lty = 2, lwd = 2)
legend(
  "topright",
  legend = c("Simulation estimate", "Theoretical value"),
  pch = c(19, NA),
  lty = c(NA, 2),
  col = c("black", "red"),
  bty = "n"
)
dev.off()
cat("\nPlot created:", file.path(script_dir, "E12_delay_runs.pdf"), "\n")
