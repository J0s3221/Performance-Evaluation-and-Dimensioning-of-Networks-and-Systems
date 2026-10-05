#This function processes the departures of packets from links

departures = function(thisLink) {

  thisPacket=Queues[[thisLink]][[1]] #Reads this packet
  thisFlow=thisPacket[1] #Reads flow of this packet
  thisRoute=Flows[[thisFlow]]$route #Reads route of this flow

  #Processes departure of this packet from this link
  lastLink=thisRoute[length(thisRoute)] #Determines the last link in the route of this flow
  if (thisLink==lastLink) { #If  departure is from last link
    thisArrivalTime=thisPacket[2] #Reads arrival time of this packet
    PacketDelay=Time-thisArrivalTime #Calculates packet delay
    FlowStats[thisFlow,1]<<-FlowStats[thisFlow,1]+1 #Increments number of packets that departed from this flow
    FlowStats[thisFlow,2]<<-FlowStats[thisFlow,2]+PacketDelay #Adds delay of this packet to delay of this flow
  }
  else { #If departure is not from last link
    nextLink=thisRoute[which(thisRoute==thisLink)+1] #Determines next link in the route of this flow
    if (length(Queues[[nextLink]])==0) {  #If queue of next link is empty
      thisLength=thisPacket[3] #Reads length of this packet
      nextLinkCapacity=LinkCapacities[nextLink] #Reads capacity of next link
      EventList<<-rbind(EventList,c(Time+thisLength/nextLinkCapacity,2,nextLink)) #Schedules departure of this packet at next link
    }
    Queues[[nextLink]][[length(Queues[[nextLink]])+1]]<<-thisPacket #Stores this packet in queue of next link
  }

  #Removes this packet from head of queue of this link
  Queues[[thisLink]][[1]]<<-NULL
  
  #Processes next packet of this link
  if (length(Queues[[thisLink]])==0) { #If queue of this link become empty
    #Do nothing
  }
  else { #If queue of this link is not empty
    nextPacket=Queues[[thisLink]][[1]] #Reads next packet of this link
    nextLength=nextPacket[3] #Reads length of next packet
    thisLinkCapacity=LinkCapacities[thisLink] #Reads capacity of this link
    EventList<<-rbind(EventList,c(Time+nextLength/thisLinkCapacity,2,thisLink)) #Schedules departure of next packet at this link
  }

}



