#Events are stored in a matrix called EventList, where each row represents an
#event. Events are defined by time of ocurrence, type of event (1-arrival at
#flow or 2-departure from link) and number of flow or link. The function sorts the
#event list, determines the next event, advances the simulation clock to the
#time of the next event, returns this event and removes it from the event list.

timing = function() {
  
  EventList<<-EventList[order(EventList[,1]),,drop=FALSE] #Sorts event list
  nextEvent=EventList[1,] #Determines next event
  Time<<-nextEvent[1] #Advances clock
  EventList<<-EventList[-1,,drop=FALSE] #Removes next event from event list
  #The argument drop=FALSE ensures that EventList remains a matrix event if thinned to just one line
  return(nextEvent)

}