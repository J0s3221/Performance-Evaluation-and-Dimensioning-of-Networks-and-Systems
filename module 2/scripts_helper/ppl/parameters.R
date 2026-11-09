
#This is the routine where the user defines the input parameters of the
#simulator

st1 = 2
st2 = 2

ar1 = 10
ar2 = 20

prior1 = 2
prior2 = 2

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
  Flows<<-list(list(sourcetype=st1,arrivalrate=ar1,packetsize=1000,priority=prior1),
               list(sourcetype=st2,arrivalrate=ar2,packetsize=1000,priority=prior2))

  #Definition of the simulation end time, function of the minimum arrival rate
  endTime<<-10000*(1/10)


  cat("Sourcetype flow 1:",st1,"\n")
  cat("Sourcetype flow 2:",st2,"\n")
  cat("arrival rate flow 1:",ar1,"\n")
  cat("arrival rate flow 2:",ar2,"\n")
  cat("priority flow 1:",prior1,"\n")
  cat("priority flow 2:",prior2,"\n")
  cat("Endtime:", endTime,"\n")
  cat("\n")
}