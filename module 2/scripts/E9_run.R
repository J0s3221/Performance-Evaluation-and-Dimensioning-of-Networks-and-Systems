# Exercise 9 - Runs the course script webserverfarm.R (file used unchanged)
# with several seeds and loads, and compares the random and JSQ task
# assignment policies with theory.
#
# webserverfarm.R has no function interface, so this script reads its code,
# replaces only the three input-parameter lines (ArrivalRate, ServiceRate,
# StoppingCondition) and runs it after set.seed().
#
# Usage (works from any folder):
#   Rscript E9_run.R               rho = 0.3 to 0.95 (6 values) x 10 seeds -> E9_results.csv + summary
#   Rscript E9_run.R 3             quick check with 3 seeds -> E9_results_quick.csv
#   Rscript E9_run.R rho seed      one run, prints one CSV line

# Location of the course script, relative to the folder of this script
WSF_FILE <- "../scripts_helper/webserverfarm.R"

RHOS   <- c(0.3, 0.5, 0.7, 0.8, 0.9, 0.95)   # system loads studied
MU     <- 1                   # service rate (the script assumes mu = 1)
NJOBS  <- 100000              # completed jobs per policy and run (script default)

# ---- Locate this script's folder and read the course script ----------------
file_arg    <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- gsub("~+~", " ", sub("^--file=", "", file_arg), fixed = TRUE)  # Rscript encodes spaces as ~+~
script_dir  <- if (length(file_arg)) dirname(normalizePath(script_path)) else getwd()
wsf_code    <- readLines(normalizePath(file.path(script_dir, WSF_FILE), mustWork = TRUE), warn = FALSE)
suppressMessages(library(extraDistr))

# ---- One run of webserverfarm.R at load rho ----------------------------------
run_once <- function(rho, seed) {
  code <- wsf_code
  code <- sub("^ArrivalRate=.*",        sprintf("ArrivalRate=%g", 2 * rho * MU), code)  # lambda = 2 rho mu
  code <- sub("^ServiceRate<<-.*",      sprintf("ServiceRate<<-%g", MU), code)
  code <- sub("^StoppingCondition=.*",  sprintf("StoppingCondition=%d", NJOBS), code)
  set.seed(seed)
  invisible(capture.output(eval(parse(text = code), envir = globalenv())))  # hide its printout
  c(random = AvgDelayRandom, jsq = AvgDelayJSQ)
}

args <- as.numeric(commandArgs(TRUE))

# ---- Single run mode -----------------------------------------------------------
if (length(args) == 2) {
  r <- run_once(args[1], args[2])
  cat(sprintf("%g,%d,%.6f,%.6f", args[1], args[2], r[1], r[2]), "\n")
  quit(save = "no")
}

# ---- Full mode -----------------------------------------------------------------
nseeds <- if (length(args) == 1) args[1] else 10
res <- NULL
for (rho in RHOS) for (seed in 1:nseeds) {
  cat(sprintf("Running rho = %.2f, seed %d\n", rho, seed))
  r <- run_once(rho, seed)
  res <- rbind(res, data.frame(rho = rho, seed = seed, random = r[1], jsq = r[2]))
}
out_file <- if (nseeds == 10) "E9_results.csv" else "E9_results_quick.csv"
write.csv(res, file.path(script_dir, out_file), row.names = FALSE)
cat("\nResults saved to", out_file, "\n")

# ---- Summary: mean of the runs with 95% Student-t CI, and theory ---------------
# Random: each server is an M/M/1-PS queue with arrival rate lambda/2 -> T = 1/(mu (1-rho))
# JSQ: no closed-form result; compared with random through the delay reduction
ci <- function(x) if (length(x) > 1) qt(0.975, length(x) - 1) * sd(x) / sqrt(length(x)) else NA
cat("\nAverage delay in system: mean of", nseeds, "runs +- 95% CI half-width\n")
for (rho in RHOS) {
  x <- res[res$rho == rho, ]
  cat(sprintf("rho=%.2f  random: theory %6.3f  sim %6.3f +- %.3f | JSQ: sim %6.3f +- %.3f | reduction %4.1f%%\n",
              rho, 1 / (MU * (1 - rho)), mean(x$random), ci(x$random),
              mean(x$jsq), ci(x$jsq), 100 * (1 - mean(x$jsq) / mean(x$random))))
}