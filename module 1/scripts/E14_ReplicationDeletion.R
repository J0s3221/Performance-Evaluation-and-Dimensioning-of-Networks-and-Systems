# Exercise 14: use the supplied frepdel() procedure to study replication counts.

service_rate <- 1
traffic_intensities <- c(0.5, 0.8, 0.95)
measurement_lengths <- c(100, 1000)
relative_errors <- c(0.05, 0.10, 0.20)
number_of_deletions <- 100
confidence_level <- 0.95
initial_replicates <- 10
random_seed <- 14

script_path <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
if (!is.null(script_path) && nzchar(script_path)) {
  script_dir <- dirname(normalizePath(script_path, winslash = "/"))
  helper_dir <- file.path(dirname(script_dir), "scripts_helper")
} else {
  roots <- unique(c(
    normalizePath(getwd(), winslash = "/"),
    dirname(normalizePath(getwd(), winslash = "/"))
  ))
  helper_candidates <- c(
    file.path(roots, "module 1", "scripts_helper"),
    file.path(roots, "scripts_helper")
  )
  helper_candidates <- helper_candidates[dir.exists(helper_candidates)]
  if (!length(helper_candidates)) stop("Could not locate module 1/scripts_helper/repdel.R")
  helper_dir <- helper_candidates[1]
  script_dir <- file.path(dirname(helper_dir), "scripts")
}

# frepdel() is kept unchanged from the supplied helper; this local FIFO version
# preserves fmm1_wu's event logic while avoiding repeated vector slicing.
helper_environment <- new.env(parent = globalenv())
source(file.path(helper_dir, "repdel.R"), local = helper_environment)
helper_environment$fmm1_wu <- function(ArrivalRate, ServiceRate, NumDeletions, NumDelays) {
  NumQueueCompleted <- 0
  ServerStatus <- 0
  NumInQueue <- 0
  AcumDelay <- 0
  QueueArrivalTime <- numeric(max(1024, NumDeletions + NumDelays))
  QueueHead <- 1
  QueueTail <- 0
  EventList <- c(rexp(1, ArrivalRate), Inf)

  while (NumQueueCompleted < NumDeletions + NumDelays) {
    NextEventType <- which.min(EventList)
    Time <- EventList[NextEventType]

    if (NextEventType == 1) {
      EventList[1] <- Time + rexp(1, ArrivalRate)
      if (ServerStatus == 1) {
        QueueTail <- QueueTail + 1
        if (QueueTail > length(QueueArrivalTime)) {
          length(QueueArrivalTime) <- max(1024, 2 * length(QueueArrivalTime))
        }
        QueueArrivalTime[QueueTail] <- Time
        NumInQueue <- NumInQueue + 1
      } else {
        NumQueueCompleted <- NumQueueCompleted + 1
        ServerStatus <- 1
        EventList[2] <- Time + rexp(1, ServiceRate)
      }
    } else if (NumInQueue == 0) {
      ServerStatus <- 0
      EventList[2] <- Inf
    } else {
      NumQueueCompleted <- NumQueueCompleted + 1
      if (NumQueueCompleted > NumDeletions) {
        AcumDelay <- AcumDelay + Time - QueueArrivalTime[QueueHead]
      }
      QueueHead <- QueueHead + 1
      NumInQueue <- NumInQueue - 1
      EventList[2] <- Time + rexp(1, ServiceRate)
    }
  }

  AcumDelay / NumDelays
}

set.seed(random_seed)
scenario_grid <- expand.grid(
  traffic_intensity = traffic_intensities,
  number_of_delays = measurement_lengths,
  relative_error = relative_errors,
  KEEP.OUT.ATTRS = FALSE
)
replication_results <- scenario_grid
replication_results$required_replicates <- mapply(
  function(traffic_intensity, number_of_delays, relative_error) {
    helper_environment$frepdel(
      gamma = relative_error,
      ArrivalRate = traffic_intensity * service_rate,
      ServiceRate = service_rate,
      NumDeletions = number_of_deletions,
      NumDelays = number_of_delays,
      conf = confidence_level,
      n0 = initial_replicates
    )
  },
  scenario_grid$traffic_intensity,
  scenario_grid$number_of_delays,
  scenario_grid$relative_error
)

variance_results <- data.frame(
  traffic_intensity = traffic_intensities,
  theoretical_mean = traffic_intensities / (service_rate * (1 - traffic_intensities)),
  theoretical_variance = traffic_intensities * (2 - traffic_intensities) /
    (service_rate^2 * (1 - traffic_intensities)^2)
)

cat("Exercise 14: helper frepdel() replication/deletion analysis\n")
cat("Service rate:", service_rate, "\n")
cat("Deleted delays per replication:", number_of_deletions, "\n")
cat("Confidence level:", confidence_level, "\n")
cat("Initial number of replications:", initial_replicates, "\n\n")
cat("Theoretical queue-delay mean and variance:\n")
print(variance_results, row.names = FALSE)
cat("\nRequired replications from frepdel():\n")
print(replication_results, row.names = FALSE)

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
    main = paste("Required replications;", measurement_length, "delays per replication")
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

cat("\nResults:", file.path(script_dir, "E14_repdel_results.csv"), "\n")
cat("Variance data:", file.path(script_dir, "E14_queue_delay_variance.csv"), "\n")
cat("Figure:", file.path(script_dir, "E14_repdel_analysis.pdf"), "\n")
