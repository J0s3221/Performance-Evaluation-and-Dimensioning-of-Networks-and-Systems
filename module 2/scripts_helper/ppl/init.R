#This routine initializes the data structures

init = function() {

  #Initialization of simulation clock
  Time<<-0
  
  #Creates event list
  EventList<<-matrix(,nrow=0,ncol=3,byrow=TRUE)
  
  #Number of flows
  numFlows<<-length(Flows)

  #Determines number of priority levels at link
  numPriorities=1
  for (i in 1:numFlows) {
    if (Flows[[i]]$priority>numPriorities)
    numPriorities=Flows[[i]]$priority
  }
  
  numQueues<<-numPriorities #There is only one queue for each priority
  
  #Creates list of empty queues
  Queues<<-rep(list(list()),numQueues)
  
  #Initializes number of packets in queue
  numPacketsInQueues<<-rep(0,numQueues)
  
  #Initializes statistical counter of each flow
  FlowStats<<-matrix(0,nrow=numFlows,ncol=3,byrow=TRUE)
  
  #Initializes link 
  TxLink<<-list() #Transmission link is empty
  LinkState<<-0 #State of transmission link is idle

  #Schedules first arrival of each flow
  for (i in 1:numFlows) {
    nextArrivalRate=Flows[[i]]$arrivalrate #Interarrival for next packet
    nextEventType=1 #Next event type is arrival
    EventList<<-rbind(EventList,c(rexp(1,nextArrivalRate),i,nextEventType))
  }

}



