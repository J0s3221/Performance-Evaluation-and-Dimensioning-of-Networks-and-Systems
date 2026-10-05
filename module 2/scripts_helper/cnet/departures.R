#This function processes the call departures of this flow

departures = function(thisFlow) {
  
  thisCallBandwidth=Flows[[thisFlow]]$bwd #Call bandwidth of this flow
  thisFlowRoute=Flows[[thisFlow]]$route #Route of this flow
  thisNumLinksFlow=length(thisFlowRoute) #Number of links in the route of this flow

  #Updates the link availability
  for (i in 1:thisNumLinksFlow) {
    LinkFlow=thisFlowRoute[i]
    LinksAvailability[LinkFlow]<<-LinksAvailability[LinkFlow]+thisCallBandwidth
  }
  
}