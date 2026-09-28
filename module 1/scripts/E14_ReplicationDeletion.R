# Exercise 14: replication/deletion precision analysis for M/M/1 queue delay.
# The first delays of every replication are discarded to reduce initialization bias.

service_rate <- 1
traffic_intensities <- c(0.5, 0.8, 0.95)
measurement_lengths <- c(100, 1000)
relative_errors <- c(0.05, 0.10, 0.20)
warmup_deletions <- 10000
minimum_replicates <- 10
maximum_replicates <- 2500
confidence_level <- 0.95
random_seed <- 14

simulate_replication_mean <- function(
  arrival_rate,
  service_rate,
  number_of_delays,
  deletion_count
) {
  target_completions <- deletion_count + number_of_delays
  service_times <- rexp(target_completions - 1, rate = service_rate)
  interarrival_times <- rexp(target_completions - 1, rate = arrival_rate)
  cumulative_workload <- cumsum(service_times - interarrival_times)
  waiting_times <- c(
    0,
    cumulative_workload - pmin(0, cummin(cumulative_workload))
  )

  mean(waiting_times[(deletion_count + 1):target_completions])
}

replication_deletion <- function(
  traffic_intensity,
  number_of_delays,
  relative_errors,
  service_rate,
  deletion_count,
  minimum_replicates,
  maximum_replicates,
  confidence_level
) {
  arrival_rate <- traffic_intensity * service_rate
  replication_means <- numeric(0)
  required_replicates <- setNames(rep(NA_integer_, length(relative_errors)), relative_errors)
  achieved_relative_errors <- setNames(rep(NA_real_, length(relative_errors)), relative_errors)
  means_at_stopping <- setNames(rep(NA_real_, length(relative_errors)), relative_errors)

  for (replication in seq_len(maximum_replicates)) {
    replication_means <- c(
      replication_means,
      simulate_replication_mean(
        arrival_rate,
        service_rate,
        number_of_delays,
        deletion_count
      )
    )

    number_completed <- length(replication_means)
    if (number_completed < minimum_replicates) {
      next
    }

    estimated_mean <- mean(replication_means)
    estimated_sd <- sd(replication_means)
    relative_half_width <- qt(
      1 - (1 - confidence_level) / 2,
      df = number_completed - 1
    ) * estimated_sd / sqrt(number_completed) / abs(estimated_mean)

    newly_satisfied <- is.na(required_replicates) & relative_half_width <= relative_errors
    if (any(newly_satisfied)) {
      required_replicates[newly_satisfied] <- number_completed
      achieved_relative_errors[newly_satisfied] <- relative_half_width
      means_at_stopping[newly_satisfied] <- estimated_mean
    }

    if (all(!is.na(required_replicates))) {
      break
    }
  }

  if (anyNA(required_replicates)) {
    stop(
      "Precision target was not reached before maximum_replicates = ",
      maximum_replicates,
      " for rho = ", traffic_intensity,
      " and measurement length = ", number_of_delays
    )
  }

  data.frame(
    traffic_intensity = traffic_intensity,
    number_of_delays = number_of_delays,
    relative_error = as.numeric(names(required_replicates)),
    required_replicates = as.integer(required_replicates),
    achieved_relative_error = as.numeric(achieved_relative_errors),
    estimated_mean = as.numeric(means_at_stopping),
    stringsAsFactors = FALSE
  )
}

set.seed(random_seed)
scenario_grid <- expand.grid(
  traffic_intensity = traffic_intensities,
  number_of_delays = measurement_lengths,
  KEEP.OUT.ATTRS = FALSE
)
replication_results <- do.call(rbind, lapply(seq_len(nrow(scenario_grid)), function(row) {
  replication_deletion(
    traffic_intensity = scenario_grid$traffic_intensity[row],
    number_of_delays = scenario_grid$number_of_delays[row],
    relative_errors = relative_errors,
    service_rate = service_rate,
    deletion_count = warmup_deletions,
    minimum_replicates = minimum_replicates,
    maximum_replicates = maximum_replicates,
    confidence_level = confidence_level
  )
}))

replication_results$theoretical_mean <- with(
  replication_results,
  traffic_intensity / (service_rate * (1 - traffic_intensity))
)
replication_results$theoretical_variance <- with(
  replication_results,
  traffic_intensity * (2 - traffic_intensity) /
    (service_rate^2 * (1 - traffic_intensity)^2)
)

variance_results <- data.frame(
  traffic_intensity = traffic_intensities,
  theoretical_mean = traffic_intensities / (service_rate * (1 - traffic_intensities)),
  theoretical_variance = traffic_intensities * (2 - traffic_intensities) /
    (service_rate^2 * (1 - traffic_intensities)^2)
)

cat("M/M/1 replication/deletion analysis\n")
cat("Service rate (mu):", service_rate, "\n")
cat("Warm-up delays deleted per replication:", warmup_deletions, "\n")
cat("Confidence level:", confidence_level, "\n")
cat("Minimum replications before testing precision:", minimum_replicates, "\n\n")
cat("Theoretical queue-delay mean and variance by traffic intensity:\n")
print(transform(
  variance_results,
  theoretical_mean = round(theoretical_mean, 4),
  theoretical_variance = round(theoretical_variance, 4)
), row.names = FALSE)
cat("\nRequired replications by precision target:\n")
print(transform(
  replication_results,
  relative_error = paste0(round(relative_error * 100), "%"),
  achieved_relative_error = round(achieved_relative_error, 4),
  estimated_mean = round(estimated_mean, 4),
  theoretical_mean = round(theoretical_mean, 4),
  theoretical_variance = round(theoretical_variance, 4)
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

write.csv(replication_results, file.path(script_dir, "E14_repdel_results.csv"), row.names = FALSE)
write.csv(variance_results, file.path(script_dir, "E14_queue_delay_variance.csv"), row.names = FALSE)

pdf(file.path(script_dir, "E14_repdel_analysis.pdf"), width = 10, height = 11)
par(mfrow = c(3, 1), mar = c(4, 5, 3, 1))
plot(
  variance_results$traffic_intensity,
  variance_results$theoretical_variance,
  type = "b",
  pch = 19,
  xlab = "Traffic intensity (rho = lambda / mu)",
  ylab = "Variance of queue delay",
  main = "Theoretical queue-delay variance (mu = 1)"
)
for (measurement_length in measurement_lengths) {
  subset_results <- replication_results[
    replication_results$number_of_delays == measurement_length,
  ]
  replication_matrix <- sapply(relative_errors, function(error_target) {
    subset_results$required_replicates[subset_results$relative_error == error_target]
  })
  matplot(
    traffic_intensities,
    replication_matrix,
    type = "b",
    pch = c(16, 17, 15),
    lty = 1,
    col = c("steelblue", "darkorange", "darkgreen"),
    xlab = "Traffic intensity (rho = lambda / mu)",
    ylab = "Required replications",
    main = paste("Required replications;", measurement_length, "measured delays per replication")
  )
  legend(
    "topright",
    legend = paste0("Relative error ", round(relative_errors * 100), "%"),
    pch = c(16, 17, 15),
    lty = 1,
    col = c("steelblue", "darkorange", "darkgreen"),
    bty = "n"
  )
}
dev.off()

cat("\nReplication results:", file.path(script_dir, "E14_repdel_results.csv"), "\n")
cat("Variance data:", file.path(script_dir, "E14_queue_delay_variance.csv"), "\n")
cat("Plots:", file.path(script_dir, "E14_repdel_analysis.pdf"), "\n")
