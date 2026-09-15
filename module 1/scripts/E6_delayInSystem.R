ArrivalRate=1
ServiceRate=2 
Time=0
NumQueueCompleted=0
ServerStatus=0
NumInQueue=0
AcumDelay=0
AcumSystemTime=0
QueueArrivalTime=c()
InServiceArrivalTime=NA   # arrival time of whoever's currently being served
EventList=c(rexp(1,ArrivalRate),Inf)

while (NumQueueCompleted<1000) {
  NextEventType=which.min(EventList)
  Time=EventList[NextEventType]
  if (NextEventType==1) {
    EventList[1]=Time+rexp(1,ArrivalRate)
    if (ServerStatus==1) {
      QueueArrivalTime=c(QueueArrivalTime,Time)
      NumInQueue=NumInQueue+1
    } else {
      ServerStatus=1
      InServiceArrivalTime=Time
      EventList[2]=Time+rexp(1,ServiceRate)
    }
  } else {
    # the customer currently in service departs now
    AcumSystemTime=AcumSystemTime+Time-InServiceArrivalTime
    NumQueueCompleted=NumQueueCompleted+1
    if (NumInQueue==0) {
      ServerStatus=0
      EventList[2]=Inf
    } else {
      AcumDelay=AcumDelay+Time-QueueArrivalTime[1]
      InServiceArrivalTime=QueueArrivalTime[1]
      QueueArrivalTime=QueueArrivalTime[-1]
      NumInQueue=NumInQueue-1
      EventList[2]=Time+rexp(1,ServiceRate)
    }
  }
}
AvgDelay=AcumDelay/NumQueueCompleted
AvgTimeInSystem=AcumSystemTime/NumQueueCompleted