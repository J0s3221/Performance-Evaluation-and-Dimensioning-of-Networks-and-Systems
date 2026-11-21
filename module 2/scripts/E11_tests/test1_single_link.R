# Exercise 11 - test 1: a single link (pure M/M/1). Same format as the
# parameters.R of pnet.R, plus the values calculated by hand (Expected).
#
# C = 1 Mb/s, mean packet length 1000 bits -> mu = 1000 pkt/s; lambda = 500 pkt/s
# By hand: T = 1/(1000-500) = 2 ms; N = 500/(1000-500) = 1; network delay = 1/500 = 2 ms
parameters <- function() {
  LinkCapacities<<-c(1e6) #In bits/sec
  Flows<<-list(list(rate=500,packetsize=1000,route=c(1)))
  endTime<<-100000*(1/500) #Only used by pnet.R
  Expected<<-list(flows=c(0.002),N=1,W=0.002)
}
