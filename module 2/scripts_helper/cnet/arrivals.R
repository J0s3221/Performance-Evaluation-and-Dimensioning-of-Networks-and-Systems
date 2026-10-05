#This function processes the call arrivals of this flow

arrivals = function (thisFlow) {

  thisArrivalRate=Flows[[thisFlow]]$rate #Arrival rate of this flow
  thisMeanCallDuration=Flows[[thisFlow]]$duration #Mean call duration of this flow
  thisCallBandwidth=Flows[[thisFlow]]$bwd #Call bandwidth of this flow
  thisFlowRoute=Flows[[thisFlow]]$route #Route of this flow
  thisNumLinksFlow=length(thisFlowRoute) #Number of links in the route of this flow

  FlowStats[thisFlow,1]<<-FlowStats[thisFlow,1]+1 #Increments number of generated calls

  #Schedules next arrival at this flow
  EventList<<-rbind(EventList,c(Time+rexp(1,thisArrivalRate),1,thisFlow))

  #Temporarily decreases available bandwidth at links of the flow path to check
  #if call can be accepted
  tempLinksAvailability=LinksAvailability
  for (i in 1:thisNumLinksFlow) {
    LinkFlow=thisFlowRoute[i]
    tempLinksAvailability[LinkFlow]=LinksAvailability[LinkFlow]-thisCallBandwidth
  }
  if (all(tempLinksAvailability>=0)) { #If call can be accepted
    LinksAvailability<<-tempLinksAvailability #Updates link availability
    EventList<<-rbind(EventList,c(Time+rexp(1,1/thisMeanCallDuration),2,thisFlow))
  } else { #If call can not be accepted
    FlowStats[thisFlow,2]<<-FlowStats[thisFlow,2]+1 #Increments number of blocked calls
  }
}