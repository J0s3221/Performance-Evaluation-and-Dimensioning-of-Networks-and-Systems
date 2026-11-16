# Exercise 7 - Runs the course simulator ppl.R (files used unchanged) with
# several seeds, and compares the average delay of each flow with theory.
#
# Usage (works from any folder):
#   Rscript E7_run.R               all 5 configurations x 10 seeds -> E7_results.csv + summary table
#   Rscript E7_run.R 3             same, with 3 seeds (quick check)
#   Rscript E7_run.R st1 st2 ar1 ar2 prior1 prior2 endTime seed    one run, prints one CSV line

# Location of the course simulator, relative to the folder of this script
PPL_DIR <- "../scripts_helper/ppl"

# ---- Locate this script's folder and load the simulator routines ----------
file_arg   <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- gsub("~+~", " ", sub("^--file=", "", file_arg), fixed = TRUE)  # Rscript encodes spaces as ~+~
script_dir <- if (length(file_arg)) dirname(normalizePath(script_path)) else getwd()
ppl_dir    <- normalizePath(file.path(script_dir, PPL_DIR), mustWork = TRUE)
for (f in c("init.R", "timing.R", "arrivals.R", "departures.R", "pq.R"))
  source(file.path(ppl_dir, f))

S <- 1000 / 100e3   # transmission time of a 1000-bit packet at 100 kb/s (s)

# ---- One simulation run: same parameters and main loop as ppl.R ----------
run_once <- function(st1, st2, ar1, ar2, prior1, prior2, endTime, seed) {
  set.seed(seed)
  LinkCapacity <<- 100e3
  Flows <<- list(list(sourcetype = st1, arrivalrate = ar1, packetsize = 1000, priority = prior1),
                 list(sourcetype = st2, arrivalrate = ar2, packetsize = 1000, priority = prior2))
  init()
  while (Time < endTime) {
    ev <- timing()
    if (ev[3] == 1) arrivals(ev[2]) else departures()
  }
  c(d1 = FlowStats[1, 2] / FlowStats[1, 1], d2 = FlowStats[2, 2] / FlowStats[2, 1],  # delay (s)
    th1 = FlowStats[1, 3] / Time, th2 = FlowStats[2, 3] / Time)                      # throughput (bit/s)
}

# ---- Theoretical delay of each flow (s) -----------------------------------
theory <- function(st, ar1, ar2, prior1, prior2) {
  lambda <- ar1 + ar2; rho <- lambda * S
  if (prior1 == prior2) {                                    # FIFO
    w <- if (st == 1) S / (1 - rho)                          # M/M/1
         else S + rho * S / (2 * (1 - rho))                  # M/D/1
    return(c(w, w))
  }
  W0 <- lambda * S^2 / 2; s1 <- ar1 * S                      # Cobham, non-preemptive M/D/1
  c(S + W0 / (1 - s1), S + W0 / ((1 - s1) * (1 - rho)))
}

args <- as.numeric(commandArgs(TRUE))

# ---- Single run mode -------------------------------------------------------
if (length(args) == 8) {
  r <- do.call(run_once, as.list(args))
  cat(paste(c(args, sprintf("%.8f", r[1:2]), sprintf("%.2f", r[3:4])), collapse = ","), "\n")
  quit(save = "no")
}

# ---- Full mode: all configurations of Table 9 x n seeds --------------------
nseeds <- if (length(args) == 1) args[1] else 10
configs <- data.frame(part = c("7a", "7a", "7b", "7b", "7c"),
                      st1 = c(1, 1, 2, 2, 2), st2 = c(1, 1, 2, 2, 2),
                      ar1 = c(10, 30, 10, 30, 10), ar2 = c(20, 60, 20, 60, 85),
                      prior1 = c(1, 1, 2, 2, 1), prior2 = c(1, 1, 2, 2, 2),
                      endTime = c(1000, 10000, 1000, 10000, 10000))

res <- NULL
for (i in seq_len(nrow(configs))) for (seed in 1:nseeds) {
  k <- configs[i, ]
  cat(sprintf("Running %s, rho = %.2f, seed %d\n", k$part, (k$ar1 + k$ar2) * S, seed))
  r <- run_once(k$st1, k$st2, k$ar1, k$ar2, k$prior1, k$prior2, k$endTime, seed)
  res <- rbind(res, data.frame(k, seed = seed, t(r)))
}
write.csv(res, file.path(script_dir, "E7_results.csv"), row.names = FALSE)

# ---- Summary: mean of the runs, 95% Student-t CI, theory ------------------
cat("\nAverage delay (ms): mean of", nseeds, "runs +- 95% CI half-width\n")
for (i in seq_len(nrow(configs))) {
  k <- configs[i, ]; x <- merge(res, k)
  th <- 1000 * theory(k$st1, k$ar1, k$ar2, k$prior1, k$prior2)
  for (f in 1:2) {
    d <- 1000 * x[[paste0("d", f)]]
    hw <- if (length(d) > 1) qt(0.975, length(d) - 1) * sd(d) / sqrt(length(d)) else NA
    cat(sprintf("%s  rho=%.2f  flow %d:  theory %7.2f   sim %7.2f +- %.2f   (rel. err. %+.2f%%)\n",
                k$part, (k$ar1 + k$ar2) * S, f, th[f], mean(d), hw, 100 * (mean(d) - th[f]) / th[f]))
  }
}