#This program simulates a circuit-switched network with fixed routing to
#estimate the blocking probability of each call flow
 
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
  nextFlow=nextEvent[3] #Flow of next event
  if (nextEventType==1) {#If next event is an arrival at flow
    arrivals(nextFlow) #Call arrival routine at specific flow
  }
  else { #If next event is a departure from a link
    departures(nextFlow) #Call departure routine at specific link
  }
}

report() #Calculate performance metrics












