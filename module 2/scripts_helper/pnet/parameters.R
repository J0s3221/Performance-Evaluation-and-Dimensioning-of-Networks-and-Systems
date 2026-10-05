parameters <- function() {

  #USER DEFINED PARAMETERS
  
  #Links must be identified by contiguous integers starting at 1. These numbers
  #must be used as indexes of the LinkCapacities vector and of the vectors
  #defining the routes of each flow. Do not use the node numbers for this
  #purpose!

  #Define here the capacity of each link
  LinkCapacities<<-c(100e6,100e6,200e6) #In bits/sec


  #Define here the flows. Flows is a list of lists that stores in each list the
  #arrival rate (in packets/second), the mean packet length (in bits) and the
  #route of each flow; the routes must be defined using the link identifiers
  #(and not the node identifiers)
  Flows<<-list(list(rate=7500,packetsize=8000,route=c(1,3)),
              list(rate=10000,packetsize=8000,route=c(2,3)),
              list(rate=5000,packetsize=8000,route=c(3)))

  #Define here the simulation end time, function of the minimum rate
  endTime<<-10000*(1/5000)

  
}