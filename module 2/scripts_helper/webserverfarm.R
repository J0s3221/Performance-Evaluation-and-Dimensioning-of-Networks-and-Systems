#Simulation of Web server farm with random and JSQ task assignment policies;
#estimates average delay in system

require(extraDistr)

#input parameters
numServers=2
ArrivalRate=1.5
ServiceRate<<-7.5
StoppingCondition=100000

rescheduleDepartures = function(thisServer,numJobsBefore,numJobsAfter) {
  for (i in 1:nrow(Departures)) {
    ElapsedTime=Time-TimePrevious[thisServer]
    if (Departures[i,4]==thisServer) {
      PreviousServicedUnits=Departures[i,3]
      LastServicedUnits=ElapsedTime*ServiceRate/numJobsBefore
      RemainingServiceUnits=PreviousServicedUnits-LastServicedUnits
      Departures[i,3]<<-RemainingServiceUnits
      Departures[i,1]<<-Time+RemainingServiceUnits*numJobsAfter/ServiceRate
    }
  }
  TimePrevious[thisServer]<<-Time
}

#Random task assignment policy
Time<<-0
nextArrivalTime=rexp(1,rate=ArrivalRate)
#Departures is a matrix of departure events where each row is an event and each
#event includes (i) departure time, (ii) arrival time, (iii) service time, and
#(iv) server number
Departures<<-matrix(rep(Inf,4),nrow=1,ncol=4,byrow=TRUE)
numJobsServer=rep(0,numServers)
TimePrevious<<-rep(0,numServers)
AcumDelay=0
NumSysCompleted=0
#Simulation loop
while (NumSysCompleted<StoppingCondition) {
  Departures<<-Departures[order(Departures[,1]),,drop=FALSE] #Sorts Departures matrix
  nextDepartureTime=Departures[1,1] #Next departure time
  Time<<-min(c(nextArrivalTime,nextDepartureTime)) #Jumps clock to time of next event
  nextEventType=which.min(c(nextArrivalTime,nextDepartureTime)) #Next event type
  if (nextEventType==1) { #Next event is arrival
    nextArrivalTime=Time+rexp(1,ArrivalRate) #Schedules next arrival
    thisServer=rdunif(1,1,numServers) #Selects next server randomly
    numJobsServer[thisServer]=numJobsServer[thisServer]+1 #Updates number of jobs running on this server
    numJobs=numJobsServer[thisServer] #Number of jobs running on this server
    if (!all(is.na(Departures))) { #Tests for empty Departures matrix
      rescheduleDepartures(thisServer,numJobs-1,numJobs) #Reschedules all departures of this server
    }
    ServiceUnits=rexp(1,rate=ServiceRate) #Service units of arriving job
    DepartureTime=Time+ServiceUnits*numJobs/ServiceRate #Departure time of arriving job
    Departures<<-rbind(Departures,c(DepartureTime,Time,ServiceUnits,thisServer)) #Inserts departure in Departures matrix 
  } else { #Next event is departure
    thisDeparture=Departures[1,] #Reads next departure from Departures matrix
    thisDelay=Time-thisDeparture[2] #Computes delay of departing job
    AcumDelay=AcumDelay+thisDelay #Accumulates delay of departing job
    thisServer=thisDeparture[4] #Server of departing job
    numJobsServer[thisServer]=numJobsServer[thisServer]-1 #Updates number of jobs running on this server
    numJobs=numJobsServer[thisServer] #Number of jobs running on this server
    NumSysCompleted=NumSysCompleted+1 #Increments number of clients that departed
    Departures<<-Departures[-1,,drop=FALSE] #Eliminates this departure from Departures matrix
    if (!all(is.na(Departures))) { #Tests for empty Departures matrix
      rescheduleDepartures(thisServer,numJobs+1,numJobs) #Reschedules all departures of this server
    }
  }
}  
AvgDelayRandom=AcumDelay/NumSysCompleted #Computes average delay

#JSQ task assignment policy
Time<<-0
nextArrivalTime=rexp(1,rate=ArrivalRate)
#Departures is a matrix of departure events where each row is an event and each
#event includes (i) departure time, (ii) arrival time, (iii) service time, and
#(iv) server number
Departures<<-matrix(rep(Inf,4),nrow=1,ncol=4,byrow=TRUE)
numJobsServer=rep(0,numServers)
TimePrevious<<-rep(0,numServers)
AcumDelay=0
NumSysCompleted=0
#Simulation loop
while (NumSysCompleted<StoppingCondition) {
  Departures<<-Departures[order(Departures[,1]),,drop=FALSE] #Sorts Departures matrix
  nextDepartureTime=Departures[1,1] #Next departure time
  Time<<-min(c(nextArrivalTime,nextDepartureTime)) #Jumps clock to time of next event
  nextEventType=which.min(c(nextArrivalTime,nextDepartureTime)) #Next event type
  if (nextEventType==1) { #Next event is arrival
    nextArrivalTime=Time+rexp(1,ArrivalRate) #Schedules next arrival
    minServers=which(numJobsServer==min(numJobsServer)) #All servers with minimum jobs
    thisServerIndex=rdunif(1,1,length(minServers)) #Selects index of server
    thisServer=minServers[thisServerIndex] #Sets server according to selected index
    numJobsServer[thisServer]=numJobsServer[thisServer]+1 #Updates number of jobs running on this server
    numJobs=numJobsServer[thisServer] #Number of jobs running on this server
    if (!all(is.na(Departures))) { #Tests for empty Departures matrix
      rescheduleDepartures(thisServer,numJobs-1,numJobs) #Reschedules all departures of this server
    }
    ServiceUnits=rexp(1,rate=ServiceRate) #Service units of arriving job
    DepartureTime=Time+ServiceUnits*numJobs/ServiceRate #Departure time of arriving job
    Departures<<-rbind(Departures,c(DepartureTime,Time,ServiceUnits,thisServer)) #Inserts departure in Departures matrix 
  } else { #Next event is departure
    thisDeparture=Departures[1,] #Reads next departure from Departures matrix
    thisDelay=Time-thisDeparture[2] #Computes delay of departing job
    AcumDelay=AcumDelay+thisDelay #Accumulates delay of departing job
    thisServer=thisDeparture[4] #Server of departing job
    numJobsServer[thisServer]=numJobsServer[thisServer]-1 #Updates number of jobs running on this server
    numJobs=numJobsServer[thisServer] #Number of jobs running on this server
    NumSysCompleted=NumSysCompleted+1 #Increments number of clients that departed
    Departures<<-Departures[-1,,drop=FALSE] #Eliminates this departure from Departures matrix
    if (!all(is.na(Departures))) { #Tests for empty Departures matrix
      rescheduleDepartures(thisServer,numJobs+1,numJobs) #Reschedules all departures of this server
    }
  }
}  
AvgDelayJSQ=AcumDelay/NumSysCompleted #Computes average delay

ro=ArrivalRate/(numServers*ServiceRate)
TrueAvgDelay=(numServers/ArrivalRate)*(ro/(1-ro))

cat(sprintf("Estimated average delay of random assignment policy= %f",AvgDelayRandom),"\n")
cat(sprintf("Estimated average delay of JSQ assignment policy= %f",AvgDelayJSQ),"\n")
cat(sprintf("True average delay of random assignment policy= %f",TrueAvgDelay))