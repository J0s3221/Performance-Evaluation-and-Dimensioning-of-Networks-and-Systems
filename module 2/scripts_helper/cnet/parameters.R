#This is the function where the user defines the simulation scenario

parameters = function() {
  
  #Links must be identified by contiguous integers starting at 1. These numbers
  #must be used as indexes of the LinkCapacities vector and of the vectors
  #defining the routes of each flow. Do not use the node numbers for this
  #purpose!
  
  #Define here the capacity of each link. The capacity may be expressed in
  #bits/sec or in number of circuits. If expressed in bits/sec, the call
  #bandwidth of each flow (bwd parameter of Flows list) must also be expressed in bits/s; if
  #expressed in number of circuits, the call bandwidth is the number of circuits
  #occupied by each call
  LinkCapacities<<-c(10,10,10,10)
  
  #Define here the flows. Flows is a list of lists that stores in each list the
  #call duration, call arrival rate, call bandwidth and the route of each flow;
  #the routes must be defined using the link identifiers (and not the node
  #identifiers)
  Flows<<-list(list(duration=2,rate=2.5,bwd=1,route=c(1,3,4)),
               list(duration=2,rate=2.5,bwd=1,route=c(2,3,4)))
  
  #Definition of the simulation end time, function of the minimum arrival rate
  endTime<<-10000*(1/2.5)
  
}