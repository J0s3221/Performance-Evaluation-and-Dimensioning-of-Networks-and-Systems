frepdel = function(gamma=0.05,ArrivalRate=1,ServiceRate=2,NumDeletions=100,
                    NumDelays=1000,conf=0.95,n0=10) {
  #Replication/deletion procedure for stopping simulations; returns number of
  #required replicas
  #gamma: relative error
  #n0: number of initial replicas
  #conf: confidence of the confidence interval
  #NumDeletions: number of delays deleted initially, in each replica
  #NumDelays: stopping condition for each replica; number of delays used to 
  #compute the actual estimate
  
  delays=c()
  for (i in 1:n0) {
    delays=c(delays,fmm1_wu(ArrivalRate,ServiceRate,NumDeletions,NumDelays))
  }
  md=mean(delays)
  tcritical=qt(1-(1-conf)/2,df=n0-1)
  vd=var(delays)
  l12=tcritical*sqrt(vd/n0)
  
  n=n0
  while (l12/md > gamma/(gamma+1)) {
    n=n+1
    delays=c(delays,fmm1_wu(ArrivalRate,ServiceRate,NumDeletions,NumDelays))
    md=mean(delays)
    tcritical=qt(1-(1-conf)/2,df=n-1)
    vd=var(delays)
    l12=tcritical*sqrt(vd/n)
  }
  
  infCI=md-l12
  supCI=md+l12
  
  #cat(sprintf("Number of replicas = %d",n),"\n")
  #cat(sprintf("Mean delay = %f",md),"\n")
  #cat(sprintf("Half-length of confidence interval = %f",l12),"\n")
  #cat(sprintf("Confidence interval is [%f,%f]",infCI,supCI),"\n")
  
  return(n)
}

fmm1_wu = function(ArrivalRate,ServiceRate,NumDeletions,NumDelays) {
  NumQueueCompleted=0
  ServerStatus=0
  NumInQueue=0
  AcumDelay=0
  QueueArrivalTime=c()
  EventList=c(rexp(1,ArrivalRate),Inf)
  while (NumQueueCompleted<(NumDeletions+NumDelays)) {
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
        NumQueueCompleted=NumQueueCompleted+1
        if (NumQueueCompleted>NumDeletions) AcumDelay=AcumDelay+Time-QueueArrivalTime[1]
        QueueArrivalTime=QueueArrivalTime[-1]
        NumInQueue=NumInQueue-1
        EventList[2]=Time+rexp(1,ServiceRate)
      }
    }
  }
  return(AcumDelay/NumDelays)
}