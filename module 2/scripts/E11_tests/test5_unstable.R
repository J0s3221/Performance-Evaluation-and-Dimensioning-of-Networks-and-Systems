# Exercise 11 - test 5: unstable network, lambda = 12.5 pkt/s > mu = 10 pkt/s (rho = 1.25)
# The script must detect it and not report (meaningless) delays.
parameters <- function() {
  LinkCapacities<<-c(10e3) #In bits/sec
  Flows<<-list(list(rate=12.5,packetsize=1000,route=c(1)))
  endTime<<-1000 #Only used by pnet.R
  Expected<<-"unstable"
}
