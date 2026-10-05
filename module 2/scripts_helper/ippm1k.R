#input parameters
PeakArrivalRate=10 #Peak arrival rate (arrival rate at state 1)
QueueSize=10 #queue capacity
Ro=0.9 #system utilization
Pi_1=0.5 #proportion of time in state 1
R=60 #ratio between peak rate and output rate of state 1
StoppingCondition=100000 #number of arrivals until simulation stops

#parameters calculated from the input parameters
q10=PeakArrivalRate/R
q01=Pi_1*q10/(1-Pi_1)
ArrivalRate=Pi_1*PeakArrivalRate
ServiceRate=ArrivalRate/Ro

#M/M/1/K
Time=0
NumArrivals=0
ServerStatus=0
NumInQueue=0
NumLoss=0
EventList=c(rexp(1,ArrivalRate),Inf)
while (NumArrivals<StoppingCondition) {
  NextEventType=which.min(EventList)
  Time=EventList[NextEventType]
  if (NextEventType==1) {
    NumArrivals=NumArrivals+1
    EventList[1]=Time+rexp(1,ArrivalRate)
    if (NumInQueue==QueueSize) {
      NumLoss=NumLoss+1
    } else if (ServerStatus==1) {
      NumInQueue=NumInQueue+1
    } else {
      ServerStatus=1
      EventList[2]=Time+rexp(1,ServiceRate)
    }
  } else {
    if (NumInQueue==0) {
      ServerStatus=0
      EventList[2]=Inf
    } else {
      NumInQueue=NumInQueue-1
      EventList[2]=Time+rexp(1,ServiceRate)
    }
  }
}
AvgLossMM1=NumLoss/NumArrivals

#IPP/M/1/K
Time=0
NumArrivals=0
ServerStatus=0
NumInQueue=0
NumLoss=0
EventList=c(rexp(1,PeakArrivalRate),Inf,rexp(1,q10))
CurrentState=1
while (NumArrivals<StoppingCondition) {
  NextEventType=which.min(EventList)
  Time=EventList[NextEventType]
  if (NextEventType==1) {
    NumArrivals=NumArrivals+1
    if (CurrentState==1) {
      EventList[1]=Time+rexp(1,PeakArrivalRate)
    }
    if (NumInQueue==QueueSize) {
      NumLoss=NumLoss+1
    } else if (ServerStatus==1) {
      NumInQueue=NumInQueue+1
    } else {
      ServerStatus=1
      EventList[2]=Time+rexp(1,ServiceRate)
    }
  } else if (NextEventType==2) {
    if (NumInQueue==0) {
      ServerStatus=0
      EventList[2]=Inf
    } else {
      NumInQueue=NumInQueue-1
      EventList[2]=Time+rexp(1,ServiceRate)
    }
  } else {
    if (CurrentState==0) { #changing to state 1
      CurrentState=1
      EventList[3]=Time+rexp(1,q10)
      EventList[1]=Time+rexp(1,PeakArrivalRate)
    } else { #changing to state 0
      CurrentState=0
      EventList[1]=Inf
      EventList[3]=Time+rexp(1,q01)
    }
  }
}
AvgLossIPPM1=NumLoss/NumArrivals

#print results
cat(sprintf("Estimated average loss M/M/1/K = %f",AvgLossMM1),"\n")
cat(sprintf("Theoretical average loss M/M/1/K = %f",(1-Ro)*Ro^(QueueSize+1)/(1-Ro^(QueueSize+2))),"\n")
cat(sprintf("Estimated average loss IPP/M/1/K = %f",AvgLossIPPM1),"\n")
