
#This is the routine where the user defines the input parameters of the
#simulator

parameters = function() {

  #Capacity of the link, in bits/sec
  LinkCapacity<<-100e3

  # Flows is a list of lists where each list corresponds to one flow, and each
  # flow has 4 elements corresponding to (1) source type, (2) arrival rate (in
  # packets/second), (3) mean packet length (in bits), and (4) priority level.
  # There are two types of sources: 1 = Poisson arrivals and exponentially
  # distributed sizes; 2 = Poisson arrivals and fixed sizes. The levels of
  # priority must be consecutive integers starting at 1, where a lower number
  # corresponds to a higher priority.
  Flows<<-list(list(sourcetype=2,arrivalrate=10,packetsize=1000,priority=1),
               list(sourcetype=2,arrivalrate=80,packetsize=1000,priority=2))

  #Definition of the simulation end time, function of the minimum arrival rate
  endTime<<-10000*(1/10)

}