#This program simulates a packet-switched network with fixed routing to estimate
#the average packet delay of each flow
  
  source("parameters.R")
  source("init.R")
  source("timing.R")
  source("arrivals.R")
  source("departures.R")
  source("report.R")
  
  parameters()

  init()
  
  while (Time < endTime){
    nextEvent=timing() #Advances simulation clock and returns next event
    nextEventType=nextEvent[2] #Type of next event
    nextResourceNumber=nextEvent[3] #Resource number of next event
    if (nextEventType==1) {#If next event is an arrival at flow
      arrivals(nextResourceNumber) #Call arrival routine at specific flow
    }
    else { #If next event is a departure from a link
      departures(nextResourceNumber) #Call departure routine at specific link
    }
  }
  
  report()