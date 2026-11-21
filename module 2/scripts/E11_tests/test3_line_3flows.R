# Exercise 11 - test 3: three nodes in line (A-B-C), three flows sharing links
#
# Link 1 = A-B, link 2 = B-C; C = 10 kb/s, 1000 bits -> mu = 10 pkt/s
# Flow 1: A-B-C (links 1,2), 4 pkt/s; flow 2: A-B (link 1), 2 pkt/s;
# flow 3: B-C (link 2), 5 pkt/s
# By hand: lambda1 = 4+2 = 6, lambda2 = 4+5 = 9
#          T1 = 1/(10-6) = 0.25 s, T2 = 1/(10-9) = 1 s
#          flow delays: 0.25+1 = 1.25 s, 0.25 s, 1 s
#          N = 6/4 + 9/1 = 10.5; gamma = 4+2+5 = 11; network delay = 10.5/11 = 0.9545 s
parameters <- function() {
  LinkCapacities<<-c(10e3,10e3) #In bits/sec
  Flows<<-list(list(rate=4,packetsize=1000,route=c(1,2)),
               list(rate=2,packetsize=1000,route=c(1)),
               list(rate=5,packetsize=1000,route=c(2)))
  endTime<<-100000*(1/2) #Only used by pnet.R
  Expected<<-list(flows=c(1.25,0.25,1),N=10.5,W=10.5/11)
}
