# Exercise 9 - Average delay in system vs load for the random and JSQ policies:
# theory for random (line) and simulation (points with 95% CI bars, from E9_results.csv).
# Run E9_run.R first. Usage (works from any folder): Rscript E9_plot.R

# ---- Locate this script's folder (files are read/written there) -----------
file_arg    <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- gsub("~+~", " ", sub("^--file=", "", file_arg), fixed = TRUE)  # Rscript encodes spaces as ~+~
script_dir  <- if (length(file_arg)) dirname(normalizePath(script_path)) else getwd()

r  <- read.csv(file.path(script_dir, "E9_results.csv"))
ci <- function(x) qt(0.975, length(x) - 1) * sd(x) / sqrt(length(x))
s  <- do.call(rbind, lapply(split(r, r$rho), function(x)
  data.frame(rho = x$rho[1], rnd = mean(x$random), rnd_hw = ci(x$random),
             jsq = mean(x$jsq), jsq_hw = ci(x$jsq))))
print(s, row.names = FALSE)

rho <- seq(0.05, 0.96, by = 0.005)
T_random <- 1 / (1 - rho)      # two independent M/M/1-PS queues (mu = 1)

jpeg(file.path(script_dir, "E9_delay_vs_rho.jpeg"), width = 900, height = 600, res = 120)
plot(rho, T_random, type = "l", lwd = 2, col = "blue", ylim = c(0, 22),
     xlab = expression(rho), ylab = "Average delay in system")
suppressWarnings(arrows(s$rho, s$rnd - s$rnd_hw, s$rho, s$rnd + s$rnd_hw, angle = 90, code = 3, length = 0.03, col = "blue"))
suppressWarnings(arrows(s$rho, s$jsq - s$jsq_hw, s$rho, s$jsq + s$jsq_hw, angle = 90, code = 3, length = 0.03, col = "red"))
points(s$rho, s$rnd, pch = 16, col = "blue")
points(s$rho, s$jsq, pch = 17, col = "red")
legend("topleft", legend = c("Random: theory",
                             "Random: simulation (95% CI)", "JSQ: simulation (95% CI)"),
       col = c("blue", "blue", "red"), lty = c(1, NA, NA),
       pch = c(NA, 16, 17), lwd = 2)
invisible(dev.off())