# Test for rho = 1 (lambda=1, mu=2), and rho = 2 (lambda=2, mu=1)
# Run 1000, 10000 and 100000 samples to get a clear estimate, without using a warm up period 
# three different values of NumQueueCompleted like requested in the exercise

ArrivalRate=1
ServiceRate=2
Time=0
NumQueueCompleted=0
ServerStatus=0
NumInQueue=0
AcumDelay=0
QueueArrivalTime=c()
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

AvgDelay=AcumDelay/NumQueueCompleted

cat("The average delay:", AvgDelay, "\n")
