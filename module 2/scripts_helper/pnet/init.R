init = function() {
  Time<<-0 #simulation clock
  
  EventList<<-matrix(,nrow=0,ncol=3,byrow=TRUE) #Creates event list
  
  numFlows<<-length(Flows) #Number of flows
  
  #Schedules first arrival of each flow
  for (i in 1:numFlows) {
    thisArrivalRate=Flows[[i]]$rate
    EventList<<-rbind(EventList,c(rexp(1,thisArrivalRate),1,i))
  }
  
  numLinks=length(LinkCapacities)
  
  Queues<<-rep(list(list()),numLinks) #Create list of empty queues
  
  #Initializes the statistical counter of each flow
  FlowStats<<-matrix(0,nrow=numFlows,ncol=2,byrow=TRUE)
}