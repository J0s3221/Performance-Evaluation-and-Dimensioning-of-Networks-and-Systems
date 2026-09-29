#Simulation of M/M/1 queuing system, to estimate the average delay in queue

#Input parameters
ArrivalRate=1
ServiceRate=2
StoppingCondition=1000

#Initialization
Time=0 #simulation clock
NumQueueCompleted=0 #number of clients that crossed the queue
ServerStatus=0 #server status (idle or busy)
NumInQueue=0 #number of clients currently in the queue
AcumDelay=0 #accumulated delay of clients
QueueArrivalTime=c() #vector storing the arrival times of clients in the queue
EventList=c(rexp(1,ArrivalRate),Inf) #event list; no departure from empty system

#Simulation loop
while (NumQueueCompleted<StoppingCondition) {
  NextEventType=which.min(EventList) #determines type of next event
  Time=EventList[NextEventType] #jumps clock to time of next event
  if (NextEventType==1) { #arrival event
    EventList[1]=Time+rexp(1,ArrivalRate) #schedules next arrival
    if (ServerStatus==1) { #client goes to queue
        QueueArrivalTime=c(QueueArrivalTime,Time) #stores arrival time of client
        NumInQueue=NumInQueue+1 #increments number of clients in the queue
      } else { #client goes directly to server
        NumQueueCompleted=NumQueueCompleted+1 #increments number of clients that have crossed the queue
        ServerStatus=1 #server becomes busy
        EventList[2]=Time+rexp(1,ServiceRate) #schedules departure of this client
      }
  }
  else { #departure event
    if (NumInQueue==0) { #departing client leaves the system empty
        ServerStatus=0 #server becomes idle
        EventList[2]=Inf #no departure from an empty system
      } else { #there are still clients in the queue when client departs
        AcumDelay=AcumDelay+Time-QueueArrivalTime[1] #calculates delay of client that leaves the queue and adds it to the accumulated delay
        QueueArrivalTime=QueueArrivalTime[-1] #removes this client from queue
        NumInQueue=NumInQueue-1 #decrements number of clients in the queue
        NumQueueCompleted=NumQueueCompleted+1 #increments number of clients that crossed the queue
        EventList[2]=Time+rexp(1,ServiceRate) #schedules departure of this client
      }
  }
}
AvgDelay=AcumDelay/NumQueueCompleted #calculates average delay in queue
print(AvgDelay) #prints the results
