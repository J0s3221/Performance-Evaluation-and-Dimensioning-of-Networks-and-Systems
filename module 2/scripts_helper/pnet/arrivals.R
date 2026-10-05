#This function processes the arrivals of packets at flows

arrivals = function (thisFlow) {

  thisArrivalRate=Flows[[thisFlow]]$rate #Reads arrival rate of this flow
  thisMeanPacketLength=Flows[[thisFlow]]$packetsize #Reads mean packet lenght of this flow
  thisRoute=Flows[[thisFlow]]$route #Reads route of this flow

  EventList<<-rbind(EventList,c(Time+rexp(1,thisArrivalRate),1,thisFlow)) #Schedules next arrival at this flow

  thisLength=rexp(1,1/thisMeanPacketLength) #Generates the length of this packet

  firstLink=thisRoute[1] #Determines first link in flow route

  if (length(Queues[[firstLink]])==0) { #If queue of first link is empty
    firstLinkCapacity=LinkCapacities[firstLink] #Determines the capacity of first link
    EventList<<-rbind(EventList,c(Time+thisLength/firstLinkCapacity,2,firstLink)) #Schedules departure of this packet at first link
  }

  thisPacket=c(thisFlow,Time,thisLength) #Builds this packet with flow number, arrival time and length
  Queues[[firstLink]][[length(Queues[[firstLink]])+1]]<<-thisPacket #Stores this packet in queue of first link

}

