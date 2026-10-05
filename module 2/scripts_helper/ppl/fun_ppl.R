# Simulator of a packet switched point-to-point link, with 2 scheduling
# mechanisms (fifo and strict priority), and 2 source types (both with Poisson
# arrivals but one with exponentially distributed packet sizes and the other
# with fixed packet sizes). It estimates the average packet delay, and the
# throughput of each flow.

funppl = function(LinkCapacity,PacketSize,ArrivalRate1,ArrivalRate2,endTime) {
  
  assign("LinkCapacity",LinkCapacity,envir=.GlobalEnv)
  
  Flows<<-list(list(sourcetype=2,arrivalrate=ArrivalRate1,packetsize=PacketSize,priority=1),
               list(sourcetype=2,arrivalrate=ArrivalRate2,packetsize=PacketSize,priority=2))
  init() #Initialization of data structures
  #Main program
  while (Time < endTime) {
    nextEvent=timing() #Next event
    nextFlow=nextEvent[2] #Flow of next event
    nextEventType=nextEvent[3] #Type of next event
    if (nextEventType==1) { #If next event is an arrival
      arrivals(nextFlow) #Call arrivals routine
    } else { #If next event is a departure
      departures() #Call departures routine
    }
  }
  return(FlowStats[2,2]/FlowStats[2,1])
}









