if (!exists("nRuns")) nRuns <- 10 # Number of independent simulation runs

# ---- Go to the folder of the course simulator --------------------------------
file_arg    <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- gsub("~+~", " ", sub("^--file=", "", file_arg), fixed = TRUE)
script_dir  <- if (length(file_arg)) dirname(normalizePath(script_path)) else getwd()
if (!exists("pnet_dir")) pnet_dir <- file.path(script_dir, "../scripts_helper/pnet")
setwd(pnet_dir)

# ---- Kleinrock approximation (same data structures as pnet.R) ---------------
# Every link is modelled as an independent M/M/1 queue.
kleinrock = function() {
  numLinks=length(LinkCapacities)
  numFlows=length(Flows)

  #Arrival rate (packets/sec) and offered load (bits/sec) of each link:
  #each flow adds its rate to every link of its route
  LinkRates=rep(0,numLinks)
  LinkBitRates=rep(0,numLinks)
  for (i in 1:numFlows) {
    for (k in Flows[[i]]$route) {
      LinkRates[k]=LinkRates[k]+Flows[[i]]$rate
      LinkBitRates[k]=LinkBitRates[k]+Flows[[i]]$rate*Flows[[i]]$packetsize
    }
  }

  rho=LinkBitRates/LinkCapacities #Link utilizations
  if (any(rho>=1)) stop("Unstable network: some link has utilization >= 1")

  mu=LinkCapacities/(LinkBitRates/LinkRates) #Service rate (packets/sec) = C / mean packet length
  LinkDelays=1/(mu-LinkRates) #M/M/1: W_k = 1/(mu-lambda)
  LinkDelays[LinkRates==0]=0 #Unused links
  LinkPackets=LinkBitRates/(LinkCapacities-LinkBitRates) #M/M/1: L_k = lambda/(mu-lambda) (as in bg.R)

  FlowDelays=sapply(Flows, function(f) sum(LinkDelays[f$route])) #(i) W_f: sum of W_k over the route
  L=sum(LinkPackets) #(ii) L: average number of packets in the network
  gamma=sum(sapply(Flows, function(f) f$rate)) #Total rate offered to the network
  W=L/gamma #(iii) W: average packet delay in the network (Little: L = gamma*W)

  cat("Kleinrock approximation\n")
  for (k in 1:numLinks)
    cat(sprintf("  Link %d: lambda = %g pkt/s, mu = %g pkt/s, rho = %.3f, L = %.3f, W = %g s\n",
                k, LinkRates[k], mu[k], rho[k], LinkPackets[k], LinkDelays[k]))
  for (i in 1:numFlows) cat(sprintf("  (i)   Average delay of flow %d, W_%d = %g s\n", i, i, FlowDelays[i]))
  cat(sprintf("  (ii)  Average number of packets in the network, L = %g\n", L))
  cat(sprintf("  (iii) Average packet delay in the network, W = %g s\n\n", W))

  list(flows=FlowDelays, L=L, W=W, gamma=gamma)
}

# ---- Theory ---------------------------------------------------------------------
source("parameters.R")
parameters() #Defines LinkCapacities, Flows and endTime
theory <- kleinrock()

# ---- Simulation with pnet.R (course simulator, unchanged) ----------------------
sim <- matrix(NA, nrow = nRuns, ncol = length(Flows) + 1) #Flow delays + network delay
for (r in 1:nRuns) {
  set.seed(r)
  invisible(capture.output(source("pnet.R"))) #Runs the simulator; results stay in FlowStats
  sim[r, ] <- c(FlowStats[, 2] / FlowStats[, 1],        #Average delay of each flow
                sum(FlowStats[, 2]) / sum(FlowStats[, 1])) #Average delay over all packets
}

# ---- Comparison: theory vs. simulation (mean and 95% confidence interval) ---------
names <- c(sprintf("W flow %d (ms)", seq_along(Flows)), "W network (ms)", "L network (packets)")
th    <- c(theory$flows, theory$W, theory$L)
simL  <- sim[, ncol(sim)] * theory$gamma #Little's law: L = gamma * W
simAll <- cbind(sim, simL)
m  <- colMeans(simAll)
hw <- qt(0.975, nRuns - 1) * apply(simAll, 2, sd) / sqrt(nRuns)
scale <- c(rep(1e3, length(Flows) + 1), 1) #Delays in ms

res <- data.frame(quantity = names,
                  kleinrock = th * scale,
                  simulation = m * scale,
                  ci95 = hw * scale,
                  error_pct = 100 * (th - m) / m)
cat(sprintf("pnet.R simulation: %d runs, endTime = %g s each\n", nRuns, endTime))
print(res, row.names = FALSE, digits = 4)

if (!exists("out_csv")) out_csv <- file.path(script_dir, "../data_results/E11_results.csv")
if (!is.null(out_csv)) {
  write.csv(res, out_csv, row.names = FALSE)
  cat("\nResults saved to", out_csv, "\n")
}
