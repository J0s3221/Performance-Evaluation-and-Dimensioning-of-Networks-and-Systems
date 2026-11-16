# Exercise 7 - Average delay vs load: theory (lines) and simulation (points,
# mean of the runs in E7_results.csv, with 95% CI bars). Run E7_run.R first.
# Usage (works from any folder):  Rscript E7_plot.R   -> E7_delay_vs_rho.jpeg

# ---- Locate this script's folder (files are read/written there) -----------
file_arg    <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- gsub("~+~", " ", sub("^--file=", "", file_arg), fixed = TRUE)  # Rscript encodes spaces as ~+~
script_dir  <- if (length(file_arg)) dirname(normalizePath(script_path)) else getwd()

S <- 0.01                                        # mean transmission time (s)
rho <- seq(0.05, 0.95, by = 0.005)
W_mm1 <- 1000 * S / (1 - rho)                    # M/M/1 delay (ms)
W_md1 <- 1000 * S * (2 - rho) / (2 * (1 - rho))  # M/D/1 delay (ms)

# ---- Simulation results: FIFO configurations only (equal priorities) -------
r <- read.csv(file.path(script_dir, "E7_results.csv"))
r <- r[r$prior1 == r$prior2, ]
r$rho <- (r$ar1 + r$ar2) * S

# Mean and 95% Student-t CI half-width per model, load and flow
sim <- do.call(rbind, lapply(split(r, list(r$st1, r$rho), drop = TRUE), function(x)
  do.call(rbind, lapply(1:2, function(f) {
    d <- 1000 * x[[paste0("d", f)]]
    data.frame(model = ifelse(x$st1[1] == 1, "mm1", "md1"), rho = x$rho[1], flow = f,
               delay = mean(d), hw = qt(0.975, length(d) - 1) * sd(d) / sqrt(length(d)))
  }))))
print(sim, row.names = FALSE)

# ---- Plot -------------------------------------------------------------------
jpeg(file.path(script_dir, "E7_delay_vs_rho.jpeg"), width = 900, height = 600, res = 120)
plot(rho, W_mm1, type = "l", lwd = 2, col = "blue", ylim = c(0, 110),
     xlab = expression(rho), ylab = "Average delay (ms)")
lines(rho, W_md1, lwd = 2, col = "red")
cols <- ifelse(sim$model == "mm1", "blue", "red")
arrows(sim$rho, sim$delay - sim$hw, sim$rho, sim$delay + sim$hw,
       angle = 90, code = 3, length = 0.03, col = cols)          # CI bars
points(sim$rho, sim$delay, pch = ifelse(sim$model == "mm1", 16, 17), col = cols, cex = 1.1)
legend("topleft", legend = c("M/M/1 theory", "M/D/1 theory",
                             "M/M/1 simulation (95% CI)", "M/D/1 simulation (95% CI)"),
       col = c("blue", "red", "blue", "red"), lty = c(1, 1, NA, NA),
       pch = c(NA, NA, 16, 17), lwd = 2)
invisible(dev.off())