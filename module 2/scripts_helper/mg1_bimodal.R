#Compares M/M/1 and M/G/1 with bimodal service distribution, in terms of average
#queuing delay, for the same arrival rate and mean service rate

#input parameters
Ro=0.5 #system utilization
s1=1;p1=0.99 #mice (size and probability)
s2=101;p2=0.01 #elephants (size and probability)
StoppingCondition=10000

#parameters calculated from the input parameters
ServiceRate=1/(s1*p1+s2*p2)
ArrivalRate=ServiceRate*Ro

#M/M/1
Time=0
NumQueueCompleted=0
ServerStatus=0
NumInQueue=0
AcumDelay=0
QueueArrivalTime=c()
EventList=c(rexp(1,ArrivalRate),Inf)
#simulation cycle
while (NumQueueCompleted<StoppingCondition) {
  NextEventType=which.min(EventList)
  Time=EventList[NextEventType]
  if (NextEventType==1) {
    EventList[1]=Time+rexp(1,ArrivalRate)
    if (ServerStatus==1) {
      QueueArrivalTime=c(QueueArrivalTime,Time)
      NumInQueue=NumInQueue+1}
    else{
      NumQueueCompleted=NumQueueCompleted+1
      ServerStatus=1
      EventList[2]=Time+rexp(1,ServiceRate)}
  }
  else {
    if (NumInQueue==0) {
      ServerStatus=0
      EventList[2]=Inf}
    else{
      AcumDelay=AcumDelay+Time-QueueArrivalTime[1]
      QueueArrivalTime=QueueArrivalTime[-1]
      NumInQueue=NumInQueue-1
      NumQueueCompleted=NumQueueCompleted+1
      EventList[2]=Time+rexp(1,ServiceRate)}
  }
}
AvgDelayMM1=AcumDelay/NumQueueCompleted

#M/G/1 Bimodal
Time=0
NumQueueCompleted=0
ServerStatus=0
NumInQueue=0
AcumDelay=0
QueueArrivalTime=c()
QueueJobSize=c()
EventList=c(rexp(1,ArrivalRate),Inf)
#simulation cycle
while (NumQueueCompleted<StoppingCondition) {
  NextEventType=which.min(EventList)
  Time=EventList[NextEventType]
  if (NextEventType==1) {
    EventList[1]=Time+rexp(1,ArrivalRate)
    JobSize=ifelse(runif(1)<p1,s1,s2)
    if (ServerStatus==1) {
      QueueArrivalTime=c(QueueArrivalTime,Time)
      QueueJobSize=c(QueueJobSize,JobSize)
      NumInQueue=NumInQueue+1
      } else{
      NumQueueCompleted=NumQueueCompleted+1
      ServerStatus=1
      EventList[2]=Time+JobSize
      }
  }
  else {
    if (NumInQueue==0) {
      ServerStatus=0
      EventList[2]=Inf
      } else {
      AcumDelay=AcumDelay+Time-QueueArrivalTime[1]
      JobSize=QueueJobSize[1]
      QueueArrivalTime=QueueArrivalTime[-1]
      QueueJobSize=QueueJobSize[-1]
      NumInQueue=NumInQueue-1
      NumQueueCompleted=NumQueueCompleted+1
      EventList[2]=Time+JobSize
      }
  }
}
AvgDelayBimodal=AcumDelay/NumQueueCompleted

#M/G/1 Pollaczek-Khinchine
es=s1*p1+s2*p2
es2=s1^2*p1+s2^2*p2
wqPK=ArrivalRate*es2/(2*(1-ArrivalRate*es))
C2PK=es2/es^2-1

cat(sprintf("Arrival rate = %f",ArrivalRate),"\n")
cat(sprintf("Service rate = %f",ServiceRate),"\n")
cat("\n")
cat(sprintf("Estimated average queuing delay M/M/1 = %f",AvgDelayMM1),"\n")
cat(sprintf("Theoretical average queuing delay M/M/1 = %f",(ArrivalRate/ServiceRate)/(ServiceRate-ArrivalRate)),"\n")
cat(sprintf("Squared Coefficient of Variation (Exponential service) = %f",1),"\n")
cat("\n")
cat(sprintf("Estimated average queuing delay M/Bimodal/1 = %f",AvgDelayBimodal),"\n")
cat(sprintf("Theoretical average queuing delay M/Bimodal/1 = %f",wqPK),"\n")
cat(sprintf("Squared Coefficient of Variation (Bimodal service) = %f",C2PK),"\n")