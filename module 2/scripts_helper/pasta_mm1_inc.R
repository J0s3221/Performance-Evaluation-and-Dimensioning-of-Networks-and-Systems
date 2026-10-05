#M/M/1 simulator that outputs (i) the average delay in system, (ii) the
#proportion of times that an arrival finds 0, 1, or 2 in the system, and (iii)
#the limiting probability that there are 0, 1, or 2 in system

#input parameters
ArrivalRate=1
ServiceRate=2

#initialization
Ro=ArrivalRate/ServiceRate
Time=0 #simulation time
TimePreviousEvent=0 #time of previous event
NumSysCompleted=0 #number of clients that crossed the system
ServerStatus=0 #server status
NumInSys=0 #number of clients currently in system
AcumDelay=0 #accumulated delay in system
SysArrivalTime=c() #store arrival times of clients in system
EventList=c(rexp(1,ArrivalRate),Inf) #event list initialization; no departure event
FindOnArrival=rep(0,3) #number of arrivals that find 0, 1 and 2 in system
TimeWithNumInSys=rep(0,3) #time with 0, 1, and 2 in system

#simulation cycle
while (NumSysCompleted<10000) {
  NextEventType=which.min(EventList) #next event type: arrival or departure
  Time=EventList[NextEventType] #jumps to time of next event
  #TO BE COMPLETED: update time with 0, 1, or 2 in system
  if (NextEventType==1) { #arrival event
    EventList[1]=Time+rexp(1,ArrivalRate) #schedule arrival event
    #TO BE COMPLETED: update number of arrivals that find 0, 1, or 2 in system
    SysArrivalTime=c(SysArrivalTime,Time) #store arrival time of arriving client
    NumInSys=NumInSys+1 #increment number in system
    if (ServerStatus==0) { #client goes directly to server
      ServerStatus=1 #server is busy
      EventList[2]=Time+rexp(1,ServiceRate) #schedule departure event
    }
  } else { #departure event
    AcumDelay=AcumDelay+Time-SysArrivalTime[1] #update accumulated delay in system
    SysArrivalTime=SysArrivalTime[-1] #remove arrival time of departing client
    NumInSys=NumInSys-1 #decrement number in system
    NumSysCompleted=NumSysCompleted+1 #increment number of clients that crossed the system
    if (NumInSys==0) { #system becomes empty
      ServerStatus=0 #server is idle
      EventList[2]=Inf #no departure from an empty system
    } else { #client is transferred from queue to server
      EventList[2]=Time+rexp(1,ServiceRate) #schedule departure event
    }
  }
  TimePreviousEvent=Time #update time of previous event
}
AvgDelaySys=AcumDelay/NumSysCompleted #average delay in system

#print results
cat(sprintf("Estimated average delay in system = %f",AvgDelaySys),"\n")
cat(sprintf("Theoretical average delay in system = %f",1/(ServiceRate-ArrivalRate)),"\n")
cat(sprintf("Estimated proportion of times that an arrival finds zero clients in system = %f",FindOnArrival[1]/NumSysCompleted),"\n")
cat(sprintf("Estimated probability of having zero clients in system = %f",TimeWithNumInSys[1]/Time),"\n")
cat(sprintf("Theoretical probability of having zero clients in system = %f",1-Ro),"\n")
cat(sprintf("Estimated proportion of times that an arrival finds one client in system = %f",FindOnArrival[2]/NumSysCompleted),"\n")
cat(sprintf("Estimated probability of having one client in system = %f",TimeWithNumInSys[2]/Time),"\n")
cat(sprintf("Theoretical probability of having one client in system = %f",(1-Ro)*Ro),"\n")
cat(sprintf("Estimated proportion of times that an arrival finds two clients in system = %f",FindOnArrival[3]/NumSysCompleted),"\n")
cat(sprintf("Estimated probability of having two clients in system = %f",TimeWithNumInSys[3]/Time),"\n")
cat(sprintf("Theoretical probability of having two clients in system = %f",(1-Ro)*Ro^2),"\n")