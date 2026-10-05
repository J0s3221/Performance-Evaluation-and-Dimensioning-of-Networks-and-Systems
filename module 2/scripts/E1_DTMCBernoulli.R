script_path <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
if (is.null(script_path) || !nzchar(script_path)) {
  script_arg <- grep("^--file=", commandArgs(), value = TRUE)
  if (length(script_arg)) script_path <- sub("^--file=", "", script_arg[1])
}
if (is.null(script_path) || !nzchar(script_path)) {
  stop("Run this file directly or source it from RStudio so its location can be determined.")
}

script_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
set.seed(5001)

simulate_comparison <- function(alpha, beta, n = 200) {
  stationary_probability <- (1 - alpha) / (1 - alpha + beta)
  dtmc <- integer(n)
  dtmc[1] <- 0L

  for (i in 2:n) {
    current <- dtmc[i - 1]
    probability_of_same_state <- if (current == 0L) alpha else 1 - beta
    dtmc[i] <- if (runif(1) < probability_of_same_state) current else 1L - current
  }

  bernoulli <- rbinom(n, size = 1, prob = stationary_probability)
  list(
    dtmc = dtmc,
    bernoulli = bernoulli,
    stationary_probability = stationary_probability
  )
}

scenarios <- data.frame(
  alpha = c(0.9, 0.1),
  beta = c(0.1, 0.9),
  label = c("Persistent states", "Alternating states"),
  stringsAsFactors = FALSE
)

summary_results <- data.frame()
for (i in seq_len(nrow(scenarios))) {
  alpha <- scenarios$alpha[i]
  beta <- scenarios$beta[i]
  result <- simulate_comparison(alpha, beta)

  file_name <- sprintf("E1_DTMCBernoulli_alpha_%s_beta_%s.jpeg", alpha, beta)
  jpeg(file.path(script_dir, file_name), width = 1200, height = 700, quality = 95)
  old_par <- par(mfrow = c(2, 1), mar = c(3, 4, 3, 1))
  plot(
    result$dtmc,
    type = "s",
    ylim = c(-0.1, 1.1),
    xlab = "Time slot",
    ylab = "State",
    main = sprintf("2-DTMC: %s (alpha = %.1f, beta = %.1f)", scenarios$label[i], alpha, beta),
    col = "navy",
    lwd = 1.5
  )
  plot(
    result$bernoulli,
    type = "s",
    ylim = c(-0.1, 1.1),
    xlab = "Time slot",
    ylab = "Value",
    main = sprintf("Bernoulli process (p = %.1f)", result$stationary_probability),
    col = "firebrick",
    lwd = 1.5
  )
  par(old_par)
  dev.off()

  summary_results <- rbind(
    summary_results,
    data.frame(
      alpha = alpha,
      beta = beta,
      stationary_probability = result$stationary_probability,
      dtmc_sample_mean = mean(result$dtmc),
      bernoulli_sample_mean = mean(result$bernoulli)
    )
  )
}

write.csv(summary_results, file.path(script_dir, "E1_DTMCBernoulli_summary.csv"), row.names = FALSE)
print(summary_results, row.names = FALSE)
