# Exercise 11 - test 4: example of the lectures (pktnet slides 14-15). The
# network and flows are the default parameters.R of pnet.R, unchanged.
#
# Link 1 = 1-3 (100 Mb/s), link 2 = 2-3 (100 Mb/s), link 3 = 3-4 (200 Mb/s); 8000 bits
# By hand: mu1 = mu2 = 12500 pkt/s, mu3 = 25000 pkt/s
#          lambda1 = 7500, lambda2 = 10000, lambda3 = 7500+10000+5000 = 22500
#          T1 = 1/5000 = 0.2 ms, T2 = 1/2500 = 0.4 ms, T3 = 1/2500 = 0.4 ms
#          flow delays: 0.6, 0.8, 0.4 ms
#          N = 1.5 + 4 + 9 = 14.5; gamma = 22500; network delay = 14.5/22500 = 0.644 ms
source(file.path(script_dir, "../scripts_helper/pnet/parameters.R")) #Course file, unchanged
pnet_parameters <- parameters
parameters <- function() {
  pnet_parameters() #Defines LinkCapacities, Flows and endTime
  Expected<<-list(flows=c(0.0006,0.0008,0.0004),N=14.5,W=14.5/22500)
}
