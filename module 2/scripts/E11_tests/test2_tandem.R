# Exercise 11 - test 2: two links in tandem, one flow (setting of exercise 12, rho = 0.5)
#
# C = 64 kb/s, mean packet length 1000 bits -> mu = 64 pkt/s; lambda = 32 pkt/s
# By hand: T1 = T2 = 1/(64-32) = 31.25 ms -> flow delay = 62.5 ms
#          N = 1 + 1 = 2; network delay = 2/32 = 62.5 ms
parameters <- function() {
  LinkCapacities<<-c(64e3,64e3) #In bits/sec
  Flows<<-list(list(rate=32,packetsize=1000,route=c(1,2)))
  endTime<<-100000*(1/32) #Only used by pnet.R
  Expected<<-list(flows=c(0.0625),N=2,W=0.0625)
}
