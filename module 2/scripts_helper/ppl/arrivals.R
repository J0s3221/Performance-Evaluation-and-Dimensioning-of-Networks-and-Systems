# This routine processes the arrivals of packets from a flow. The routine
# generates a packet, enques it and calls the packet scheduler. Packets have
# atributes flow number, arrival time and length. The routine also schedules the
# next packet arrival of this flow.

arrivals = function(thisFlow) {

  #Generates this packet
  SourceType=Flows[[thisFlow]]$sourcetype #Source type of this flow
  if (SourceType==1) { #If source type is 1 
    thisLength=rexp(1,rate=1/Flows[[thisFlow]]$packetsize) #Packet size is exponentially distributed
  } else { #If source type is 2 
    thisLength=Flows[[thisFlow]]$packetsize #Packet size is fixed
  }
  thisPacket=c(thisFlow,Time,thisLength) #Builds this packet

  #Schedules next arrival of this flow
  nextInterarrival=rexp(1,rate=Flows[[thisFlow]]$arrivalrate) #Interarrival for next packet
  nextArrivalTime=Time+nextInterarrival #Time of next arrival
  nextEventType=1 #Next event type is arrival
  EventList<<-rbind(EventList,c(nextArrivalTime,thisFlow,nextEventType)) #Places arrival event in event list
  
  #Enques this packet
  thisPriority=Flows[[thisFlow]]$priority
  Queues[[thisPriority]][[length(Queues[[thisPriority]])+1]]<<-thisPacket
  numPacketsInQueues[thisPriority]<<-numPacketsInQueues[thisPriority]+1

  #Calls packet scheduler if link is idle
  if (LinkState==0) pq()

}