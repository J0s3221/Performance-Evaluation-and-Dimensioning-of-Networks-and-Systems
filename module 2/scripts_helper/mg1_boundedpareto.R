#Compares M/M/1 and M/G/1 with Bounded Pareto service distribution, in terms of
#average queuing delay, for the same arrival rate and mean service rate

library(VGAM)

#input parameters
Ro=0.5
L=0.286
H=10^6
alpha=1.4
StoppingCondition=100000

#parameters calculated from the input parameters
meanBP=L^alpha/(1-(L/H)^alpha)*(alpha/(alpha-1)*(1/L^(alpha-1)-1/H^(alpha-1)))
varBP=L^alpha/(1-(L/H)^alpha)*(alpha/(alpha-2)*(1/L^(alpha-2)-1/H^(alpha-2)))
ServiceRate=1/meanBP
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

#M/BoundedPareto/1
Time=0
NumQueueCompleted=0
ServerStatus=0
NumInQueue=0
AcumDelay=0
QueueArrivalTime=c()
EventList=c(rexp(1,ArrivalRate),Inf)
#simulation loop
while (NumQueueCompleted<StoppingCondition) {
  NextEventType=which.min(EventList)
  Time=EventList[NextEventType]
  if (NextEventType==1) {
    EventList[1]=Time+rexp(1,ArrivalRate)
    if (ServerStatus==1) {
      QueueArrivalTime=c(QueueArrivalTime,Time)
      NumInQueue=NumInQueue+1
      } else{
      NumQueueCompleted=NumQueueCompleted+1
      ServerStatus=1
      EventList[2]=Time+rtruncpareto(1,lower=L,upper=H,shape=alpha)
      }
  }
  else {
    if (NumInQueue==0) {
      ServerStatus=0
      EventList[2]=Inf
      } else{
      AcumDelay=AcumDelay+Time-QueueArrivalTime[1]
      QueueArrivalTime=QueueArrivalTime[-1]
      NumInQueue=NumInQueue-1
      NumQueueCompleted=NumQueueCompleted+1
      EventList[2]=Time+rtruncpareto(1,lower=L,upper=H,shape=alpha)
      }
  }
}
AvgDelayBoundedPareto=AcumDelay/NumQueueCompleted

#M/G/1 Pollaczek-Khinchine
es=meanBP
es2=varBP+meanBP^2
wqPK=ArrivalRate*es2/(2*(1-ArrivalRate*es))
C2PK=varBP/meanBP^2

cat(sprintf("Arrival rate = %f",ArrivalRate),"\n")
cat(sprintf("Service rate = %f",ServiceRate),"\n")
cat("\n")
cat(sprintf("Estimated average queuing delay M/M/1 = %f",AvgDelayMM1),"\n")
cat(sprintf("Theoretical average queuing delay M/M/1 = %f",(ArrivalRate/ServiceRate)/(ServiceRate-ArrivalRate)),"\n")
cat(sprintf("Squared Coefficient of Variation (Exponential service) = %f",1),"\n")
cat("\n")
cat(sprintf("Estimated average queuing delay M/BoundedPareto/1 = %f",AvgDelayBoundedPareto),"\n")
cat(sprintf("Theoretical average queuing delay M/BoundedPareto/1 = %f",wqPK),"\n")
cat(sprintf("Squared Coefficient of Variation (Bounded Pareto service) = %f",C2PK),"\n")
