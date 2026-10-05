# This routine processes departures at the transmission link. It calculates the
# delay of the packet that departs and updates the statistical counters.
# Afterwards it calls the packet scheduler that will determine the next packet
# to be transmitted in the link.

departures = function() {

  thisPacket=TxLink #Packet that ended transmission
  
  thisFlow=thisPacket[1] #Flow of this packet

  #Calculates the delay of this packet
  thisArrivalTime=thisPacket[2] #Arrival time of this packet
  thisPacketDelay=Time-thisArrivalTime #Calculates packet delay

  #Updates statistical counters
  thisLength=thisPacket[3]
  FlowStats[thisFlow,1]<<-FlowStats[thisFlow,1]+1 #Increments number of packets that arrived at destination
  FlowStats[thisFlow,2]<<-FlowStats[thisFlow,2]+thisPacketDelay #Adds delay of this packet to flow delay
  FlowStats[thisFlow,3]<<-FlowStats[thisFlow,3]+thisLength #Adds length of this packet to total number of completed bits

  #Removes this packet from the link
  TxLink<<-c()
  LinkState<<-0 #Set link state to idle

  #Calls packet scheduler if there are packets waiting
  if (any(numPacketsInQueues>0)) pq()

}