# Exercise 13 - Packet switched network of Figure 3 (all links 256 kb/s,
# mean packet size 1000 bits -> mu = 256 packets/s on every link).
#
# Links (pnet.R numbering):
#   1: 1->2   2: 1->3   3: 2->4   4: 2->5   5: 3->5   6: 4->6   7: 5->6
# Flows (non-bifurcated fixed routing):
#   flow 1: 1-2-4-6 (links 1,3,6), 215 pkt/s   flow 2: 1-3-5   (links 2,5),    64 pkt/s
#   flow 3: 1-3-5-6 (links 2,5,7), 128 pkt/s   flow 4: 2-5     (link 4),      128 pkt/s
#
# a) + b) E11_Kleinrock.R is run on this network: Kleinrock approximation and
#         nRuns simulations with pnet.R (parameters.R written in a temporary
#         copy of scripts_helper/pnet; the course files are not modified).
# c)      Optimal bifurcation of flow 1 over 1-2-4-6 and 1-2-5-6: hand
#         calculation (equal first derivative lengths), checked with the
#         kleinrock() function of E11 and with a pnet.R simulation.
# d)      All flows can be bifurcated: bg.R (gradient projection) is run with
#         its parameters.R, unchanged (it already describes this network).
#
# Usage: Rscript E13_PacketNetwork.R      (from any folder; takes a few minutes)

endTime13 <- 100   # Simulation time of each pnet.R run (s): about 53 500 packets
nRuns13   <- 10    # Independent runs per simulated scenario

# ---- Folders ----------------------------------------------------------------
file_arg    <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- gsub("~+~", " ", sub("^--file=", "", file_arg), fixed = TRUE)
script_dir  <- if (length(file_arg)) dirname(normalizePath(script_path)) else getwd()
course_pnet <- normalizePath(file.path(script_dir, "../scripts_helper/pnet"))
course_bg   <- normalizePath(file.path(script_dir, "../scripts_helper/bg"))

# ---- parameters.R in the pnet.R format, for a given list of flows ---------------
params_text = function(flows, comment) {
  fl <- sapply(flows, function(f) sprintf("list(rate=%g,packetsize=1000,route=c(%s))",
                                         f$rate, paste(f$route, collapse = ",")))
  sprintf('parameters <- function() {
  #Exercise 13: %s
  #Links: 1:1->2 2:1->3 3:2->4 4:2->5 5:3->5 6:4->6 7:5->6
  LinkCapacities<<-rep(256e3,7) #In bits/sec
  Flows<<-list(%s)
  endTime<<-%g #In seconds
}
', comment, paste(fl, collapse = ",\n               "), endTime13)
}

# ---- Runs E11_Kleinrock.R on a network; returns its comparison table -----------
run_E11 = function(flows, comment) {
  txt <- params_text(flows, comment)
  cat("\nparameters.R used:\n", txt, "\n")
  pnet_dir <<- tempfile("pnet"); dir.create(pnet_dir)
  file.copy(list.files(course_pnet, pattern = "\\.R$", full.names = TRUE), pnet_dir)
  writeLines(txt, file.path(pnet_dir, "parameters.R"))
  nRuns   <<- nRuns13
  out_csv <<- NULL
  source(file.path(script_dir, "E11_Kleinrock.R")) #Theory + simulation, table in 'res'
  setwd(script_dir)
  unlink(pnet_dir, recursive = TRUE)
  res
}

lambda <- c(215, 64, 128, 128)   # Rates of flows 1..4 (packets/s)
gamma  <- sum(lambda)            # Total rate offered to the network = 535 packets/s
mu     <- 256e3 / 1000           # Service rate of every link = 256 packets/s

# ======================== a) and b) ==============================================
cat("\n#################### a) Kleinrock and b) pnet.R simulation ####################\n")
flows_a <- list(list(rate = 215, route = c(1, 3, 6)),
                list(rate = 64,  route = c(2, 5)),
                list(rate = 128, route = c(2, 5, 7)),
                list(rate = 128, route = c(4)))
res_a <- run_E11(flows_a, "questions a) and b), non-bifurcated routing")

# ======================== c) =======================================================
cat("\n#################### c) Optimal bifurcation of flow 1 ####################\n")
# x pkt/s of flow 1 on 1-2-4-6 (links 1,3,6) and 215-x on 1-2-5-6 (links 1,4,7).
# Links 3 and 6 carry x; links 4 and 7 carry 128 + (215-x) = 343-x (flow 4 is on
# link 4 and flow 3 on link 7, 128 pkt/s each); link 1 carries 215 in any case.
# The optimum has equal first derivative lengths of the two routes (link 1 is
# common and cancels out), with dL_k/dlambda_k = mu/(mu-lambda_k)^2:
#   2*mu/(mu-x)^2 = 2*mu/(mu-(343-x))^2  ->  x = 343 - x  ->  x = 171.5 pkt/s
x_opt <- (215 + 128) / 2
cat(sprintf("Hand calculation: x = %g pkt/s on 1-2-4-6 and 215 - x = %g pkt/s on 1-2-5-6\n",
            x_opt, 215 - x_opt))

# Check with the kleinrock() function of E11 (defined when E11 was sourced above)
W_of_x = function(x) {
  LinkCapacities <<- rep(256e3, 7)
  Flows <<- list(list(rate = x,       packetsize = 1000, route = c(1, 3, 6)),
                 list(rate = 215 - x, packetsize = 1000, route = c(1, 4, 7)),
                 list(rate = 64,      packetsize = 1000, route = c(2, 5)),
                 list(rate = 128,     packetsize = 1000, route = c(2, 5, 7)),
                 list(rate = 128,     packetsize = 1000, route = c(4)))
  invisible(capture.output(k <- kleinrock()))
  k$W
}
xs <- seq(88, 215, by = 0.5)  # Feasible range: links 4 and 7 need 343-x < 256 -> x > 87
Ws <- sapply(xs, W_of_x)
x_num <- optimize(W_of_x, c(87.01, 215))$minimum
cat(sprintf("Numerical check (minimum of W(x) with kleinrock()): x = %.2f pkt/s\n", x_num))

png(file.path(script_dir, "../images/E13_bifurcation.png"), width = 1600, height = 1000, res = 220)
plot(xs, 1e3 * Ws, type = "l", lwd = 2, xlim = c(110, 215), ylim = c(30, 70), xlab = "Rate of flow 1 on route 1-2-4-6, x (packets/s)",
     ylab = "Average network packet delay W (ms)", main = "Exercise 13 c): bifurcation of flow 1")
abline(v = x_opt, lty = 2); points(x_opt, 1e3 * W_of_x(x_opt), pch = 19)
points(215, 1e3 * W_of_x(215), pch = 1)
legend("topright", c("optimum (x = 171.5)", "question a) (x = 215)"), pch = c(19, 1), bty = "n")
dev.off()

# Kleinrock + simulation of the optimal bifurcation (flow 1 split in two flows)
flows_c <- list(list(rate = x_opt,       route = c(1, 3, 6)),
                list(rate = 215 - x_opt, route = c(1, 4, 7)),
                list(rate = 64,          route = c(2, 5)),
                list(rate = 128,         route = c(2, 5, 7)),
                list(rate = 128,         route = c(4)))
res_c <- run_E11(flows_c, "question c), flow 1 bifurcated (flows 1a and 1b)")

# ======================== d) =======================================================
cat("\n#################### d) All flows bifurcated: bg.R ####################\n")
cat("bg.R parameters.R (course file, unchanged):\n")
cat(readLines(file.path(course_bg, "parameters.R")), sep = "\n")
cat("\nbg.R output:\n")
setwd(course_bg)
bg_out <- capture.output(source("bg.R"))
setwd(script_dir)
cat(bg_out, sep = "\n")
L_d <- TotalMeanPackets          # Left by bg.R: average number of packets in the network
W_d <- L_d / gamma               # Little: W = L / gamma
cat(sprintf("\nW = L / gamma = %f / %g = %f s\n", L_d, gamma, W_d))

# Simulation of the bg.R solution with pnet.R (each route of each OD pair as one flow)
link_id <- matrix(0, 6, 6)
link_id[1, 2] <- 1; link_id[1, 3] <- 2; link_id[2, 4] <- 3; link_id[2, 5] <- 4
link_id[3, 5] <- 5; link_id[4, 6] <- 6; link_id[5, 6] <- 7
flows_d <- list()
for (od in Paths) for (p in od) if (p$rate > 1e-6) {
  r <- as.vector(p$route)
  flows_d[[length(flows_d) + 1]] <- list(rate = p$rate / 1000,  # bits/s -> packets/s
                                         route = link_id[cbind(r[-length(r)], r[-1])])
}
res_d <- run_E11(flows_d, "question d), bifurcation given by bg.R")

# ======================== Summary =================================================
row = function(res, q) res[res$quantity == q, ]
summary13 <- data.frame(
  question = c("a/b) fixed routing", "c) flow 1 bifurcated", "d) all flows bifurcated (bg.R)"),
  L_kleinrock = c(row(res_a, "L network (packets)")$kleinrock,
                  row(res_c, "L network (packets)")$kleinrock, L_d),
  W_kleinrock_ms = c(row(res_a, "W network (ms)")$kleinrock,
                     row(res_c, "W network (ms)")$kleinrock, 1e3 * W_d),
  W_sim_ms = c(row(res_a, "W network (ms)")$simulation,
               row(res_c, "W network (ms)")$simulation,
               row(res_d, "W network (ms)")$simulation),
  ci95_ms = c(row(res_a, "W network (ms)")$ci95,
              row(res_c, "W network (ms)")$ci95,
              row(res_d, "W network (ms)")$ci95))
cat("\n\n#################### Summary ####################\n")
print(summary13, row.names = FALSE, digits = 4)

write.csv(res_a, file.path(script_dir, "../data_results/E13_ab_results.csv"), row.names = FALSE)
write.csv(res_c, file.path(script_dir, "../data_results/E13_c_results.csv"), row.names = FALSE)
write.csv(res_d, file.path(script_dir, "../data_results/E13_d_results.csv"), row.names = FALSE)
write.csv(summary13, file.path(script_dir, "../data_results/E13_summary.csv"), row.names = FALSE)
cat("\nResults saved to data_results/E13_*.csv and images/E13_bifurcation.png\n")
