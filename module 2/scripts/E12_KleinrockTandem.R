# Exercise 12 - Accuracy of the Kleinrock approximation: two tandem links.
#
# Network: two links of 64 kb/s in tandem; one flow from the first to the last
# node, Poisson arrivals, exponential packet sizes with mean 1000 bits.
# So mu = 64000/1000 = 64 packets/s and lambda = rho * mu.
#
# This script reuses E11_Kleinrock.R. For each rho it only defines the network:
# it writes the parameters.R of that rho in a temporary copy of
# scripts_helper/pnet (the course files are not modified). Then it runs
# E11_Kleinrock.R on that copy, which computes the Kleinrock approximation and
# runs pnet.R nRuns times, and keeps the comparison of the average packet delay.
#
# Usage: Rscript E12_KleinrockTandem.R      (from any folder; takes ~30 min)

rhos       <- c(0.05, 0.5, 0.95, 0.975)  # Loads asked in the exercise
numPackets <- c(1e5, 1e5, 5e5, 1e6)      # Approx. packets per run (longer runs for high rho)
mu         <- 64e3 / 1000                # Service rate of each link (packets/s)

# ---- Folders ----------------------------------------------------------------
file_arg    <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- gsub("~+~", " ", sub("^--file=", "", file_arg), fixed = TRUE)
script_dir  <- if (length(file_arg)) dirname(normalizePath(script_path)) else getwd()
course_pnet <- normalizePath(file.path(script_dir, "../scripts_helper/pnet"))

# ---- parameters.R for a given rho (same format as the course file) -------------
params_text = function(rho, nPk) {
  lambda <- rho * mu
  sprintf('parameters <- function() {
  #Exercise 12: two tandem links of 64 kb/s, rho = %g
  LinkCapacities<<-c(64e3,64e3) #In bits/sec
  Flows<<-list(list(rate=%g,packetsize=1000,route=c(1,2))) #lambda = rho*mu
  endTime<<-%g*(1/%g) #About %g packets
}
', rho, lambda, nPk, lambda, nPk)
}

# ---- Experiment: E11_Kleinrock.R for each rho ------------------------------------
table12 <- NULL
for (i in seq_along(rhos)) {
  rho <- rhos[i]
  cat(sprintf("\n=================== rho = %g ===================\n", rho))

  #Temporary copy of the course simulator with the parameters.R of this rho
  pnet_dir <- tempfile("pnet"); dir.create(pnet_dir)
  file.copy(list.files(course_pnet, pattern = "\\.R$", full.names = TRUE), pnet_dir)
  writeLines(params_text(rho, numPackets[i]), file.path(pnet_dir, "parameters.R"))

  #Runs Exercise 11 on this network (theory + nRuns pnet.R runs); result in 'res'
  nRuns   <- 10
  out_csv <- NULL
  source(file.path(script_dir, "E11_Kleinrock.R"))
  setwd(script_dir)
  unlink(pnet_dir, recursive = TRUE)

  #Keeps the line of the average packet delay of the (single) flow
  row <- res[res$quantity == "W flow 1 (ms)", ]
  table12 <- rbind(table12, data.frame(rho = rho, lambda = rho * mu,
                                       endTime = numPackets[i] / (rho * mu),
                                       W_kleinrock_ms = row$kleinrock,
                                       W_sim_ms = row$simulation,
                                       ci95_ms = row$ci95,
                                       error_pct = row$error_pct))
}

# ---- Table of the exercise ----------------------------------------------------------
cat("\n\nExercise 12 - two tandem links of 64 kb/s, mean packet 1000 bits (mu = 64 pkt/s)\n")
print(table12, row.names = FALSE, digits = 4)
write.csv(table12, file.path(script_dir, "../data_results/E12_results.csv"), row.names = FALSE)
cat("\nResults saved to data_results/E12_results.csv\n")
