#Simulator of packet scheduler with two queues, using a random scheduling
#algorithm. The queues have independent Poisson arrivals and uniformly
#distributed packet sizes

#Input parameters
ArrivalRate=c(1,1) #Arrival rate at each queue
AvgPacketSize=c(1000,3000) #Average packet size of packets arriving at each queue
LinkCapacity=1000 #Link capacity

#Initialization
Time=0 #Simulation clock
NumSysCompleted=0 #Number of packets that crossed the system
LinkStatus=0 #Link status (idle or busy)
NumInQueue=c(0,0) #Number of packets in each queue
QueuePacketSize=list(c(),c()) #Initializes list storing packet sizes at each queue
AcumBits=c(0,0) #Accumulator of bits transmitted from each queue
EventList=c(rexp(1,ArrivalRate[1]),rexp(1,ArrivalRate[2]),Inf) #Event list initialization

#Simulation loop
while (NumSysCompleted<10000) {
  NextEventType=which.min(EventList) #Determines next event
  Time=EventList[NextEventType] #Jumps clock to time of next event
  if (NextEventType==1 || NextEventType==2) { #Arrival events at queue 1 or queue 2
    EventList[NextEventType]=Time+rexp(1,ArrivalRate[NextEventType]) #Next arrival to queue
    PacketSize=sample(seq(AvgPacketSize[NextEventType]-500,AvgPacketSize[NextEventType]+500,100),1) #Define packet size of arriving packet
    if (LinkStatus==1) { #Link is busy
      QueuePacketSize[[NextEventType]]=c(QueuePacketSize[[NextEventType]],PacketSize) #Queues arriving packet
      NumInQueue[NextEventType]=NumInQueue[NextEventType]+1 #Increments number of packets in this queue
    }
    else { #Link not busy
      LinkStatus=1 #Link becomes busy
      ServedPacketSize=PacketSize #Places packet in link (its packet size)
      ServedQueue=NextEventType #Places packet in link (its queue)
      EventList[3]=Time+ServedPacketSize/LinkCapacity #Schedules departure of this packet
    }
   } else { #Departure event
      NumSysCompleted=NumSysCompleted+1 #Increments number of packets that crossed the system
      AcumBits[ServedQueue]=AcumBits[ServedQueue]+ServedPacketSize #Accumulates bits of served packet
      if (all(NumInQueue==0)) { #System is empty
        EventList[3]=Inf #No departure from empty system
        LinkStatus=0 #Link becomes idle
      } else if (any(NumInQueue==0)) { #Only one queue empty
        CurrentQueue=which(NumInQueue!=0) #Discovers non-empty queue
        PacketSize=QueuePacketSize[[CurrentQueue]][1] #Reads packet size of non-empty queue
        ServedPacketSize=PacketSize #Places packet in link (its packet size)
        ServedQueue=CurrentQueue #Places packet in link (its queue)
        QueuePacketSize[[CurrentQueue]]=QueuePacketSize[[CurrentQueue]][-1] #Removes packet size from non-empty queue
        NumInQueue[CurrentQueue]=NumInQueue[CurrentQueue]-1 #Updates number of clients in non-empty queue
        EventList[3]=Time+ServedPacketSize/LinkCapacity #Schedules departure of this packet
      } else { #No queue empty
        CurrentQueue=sample(c(1,2),1) #Selects randomly queue of next packet to be transmitted
        ServedPacketSize=QueuePacketSize[[CurrentQueue]][1] #Places packet in link (its packet size)
        ServedQueue=CurrentQueue #Places packet in link (its queue)
        QueuePacketSize[[CurrentQueue]]=QueuePacketSize[[CurrentQueue]][-1] #Removes packet size from current queue
        NumInQueue[CurrentQueue]=NumInQueue[CurrentQueue]-1 #Updates number of clients in current queue
        EventList[3]=Time+ServedPacketSize/LinkCapacity #Schedules departure of this packet
      }
   }
}

#Print results
sprintf("The throughput of queue 1 is %f",AcumBits[1]/Time)
sprintf("The throughput of queue 2 is %f",AcumBits[2]/Time)

