fmm1_initcond = function(ArrivalRate,ServiceRate,NumEndClients,InitClients) {
  #Estimates the average delay in queue of an M/M/1 queuing system, with the
  #possibility of defining initial conditions
  #NumEndClients: stopping condition; number of clients that traversed the queue
  #InitClients: number of clients initially in the system
  NumQueueCompleted=0
  ServerStatus=ifelse(InitClients==0,0,1)
  NumInQueue=ifelse(InitClients==0,0,InitClients-1)
  AcumDelay=0
  Time=0
  QueueArrivalTime=c()
  if (InitClients==0) {
    EventList=c(rexp(1,Time+ArrivalRate),Inf)
  } else {
    for (i in 1:(InitClients-1)) {
      Time=Time+rexp(1,ArrivalRate)
      QueueArrivalTime=c(QueueArrivalTime,Time)
    }
    EventList=c(Time+rexp(1,ArrivalRate),Time+rexp(1,ServiceRate))
  }
  while (NumQueueCompleted<NumEndClients) {
    NextEventType=which.min(EventList)
    Time=EventList[NextEventType]
    if (NextEventType==1) {
      EventList[1]=Time+rexp(1,ArrivalRate)
      if (ServerStatus==1) {
        QueueArrivalTime=c(QueueArrivalTime,Time)
        NumInQueue=NumInQueue+1
      } else {
        NumQueueCompleted=NumQueueCompleted+1
        ServerStatus=1
        EventList[2]=Time+rexp(1,ServiceRate)
      }
    } else {
      if (NumInQueue==0) {
        ServerStatus=0
        EventList[2]=Inf
      } else {
        AcumDelay=AcumDelay+Time-QueueArrivalTime[1]
        QueueArrivalTime=QueueArrivalTime[-1]
        NumInQueue=NumInQueue-1
        NumQueueCompleted=NumQueueCompleted+1
        EventList[2]=Time+rexp(1,ServiceRate)
      }
    }
  }
AcumDelay/NumQueueCompleted
}
