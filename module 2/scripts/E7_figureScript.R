S <- 0.01                                    # mean transmission time (s)
rho <- seq(0.05, 0.9, by = 0.005)
W_mm1 <- 1000 * S / (1 - rho)                # ms
W_md1 <- 1000 * S * (2 - rho) / (2 * (1 - rho))

sim <- data.frame(rho   = c(0.3, 0.3, 0.3, 0.3),
                  model = c("mm1", "mm1", "md1", "md1"),
                  delay = c(14.473, 14.836, 12.047, 12.090))   # add rho = 0.9 rows

jpeg("E7_delay_vs_rho.jpeg", width = 900, height = 600, res = 120)
plot(rho, W_mm1, type = "l", lwd = 2, col = "blue", ylim = c(0, 110),
     xlab = expression(rho), ylab = "Average delay (ms)")
lines(rho, W_md1, lwd = 2, col = "red")
points(sim$rho, sim$delay, pch = ifelse(sim$model == "mm1", 16, 17),
       col = ifelse(sim$model == "mm1", "blue", "red"), cex = 1.3)
legend("topleft", legend = c("M/M/1 theory", "M/D/1 theory", "M/M/1 simulation", "M/D/1 simulation"),
       col = c("blue", "red", "blue", "red"), lty = c(1, 1, NA, NA), pch = c(NA, NA, 16, 17), lwd = 2)
dev.off()