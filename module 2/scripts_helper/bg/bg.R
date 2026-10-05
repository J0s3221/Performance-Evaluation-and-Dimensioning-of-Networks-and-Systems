#This script determines the optimal flow bifurcations that minimize the network
#average packet delay, using the Bertsekas algorithm (section 5.7 of Data
#Networks book)

  source("parameters.R")
  source("shortestpath.R")
  source("aux_bg.R")

  parameters() #Calls parameters routine

  NumNodes=ncol(LinkCapacities) #Number of nodes
  NumLinks=sum(LinkCapacities!=0) #Number of links
  NumODPairs=nrow(Flows) #Number of flows

  #Alpha=NumLinks/NumNodes^2
  Alpha=1 #Stepsize for flow deviation
  DeltaCap=0.05 #Capacity adjusting factor, for phase I
  ErrorPhase2=10^(-8) #Convergence tolerance phase II

  LinkRates=matrix(rep(0,NumNodes^2),nrow=NumNodes) #Initializes LinkRates matrix with all zeros
  
  LinkFirstDerivativeCosts=1/LinkCapacities #Initial link costs with all link rates equal to zero
  #print(LinkFirstDerivativeCosts)
  
  Paths=rep(list(list(list(route=c(),rate=NULL))),NumODPairs) #Initializes Path list

  #Initial solution - each flow routed through shortest path based on link first derivative costs
  for (i in 1:NumODPairs) { 
    Origin=Flows[i,1] #Origin of flow
    Destination=Flows[i,2] #Destination of flow
    Rate=Flows[i,3] #Rate of flow
    Route=shortestpath(LinkFirstDerivativeCosts,Origin,Destination) #Shortest path of flow
    Paths[[i]][[1]]=list(route=Route,rate=Rate) #Inserts path information in Paths list
  }
  
  #Loads links with rate
  LinkRates=loadlinks(NumNodes,NumODPairs,Paths)
  #print("LinkRates");print(LinkRates)
  
  #Determines maximum link load (ignoring NaN)
  MaxUtilization=max(LinkRates/LinkCapacities,na.rm=TRUE)
  #print("MaxUtilization");print(MaxUtilization)

  #print("Paths");print(Paths)
  
  #Phase I: finds feasible solution
  niter1=0 #Initializes number of iterations of phase I
  while (MaxUtilization>=1) { #While there is at least one link overloaded
    CapacityAdjustFactor=(1+DeltaCap)*MaxUtilization #Determines capacity adjust factor
    #print("CapacityAdjustFactor");print(CapacityAdjustFactor)
    AdjustedLinkCapacities=CapacityAdjustFactor*LinkCapacities #Determines adjusted link capacities
    #print("AdjustedLinkCapacities");print(AdjustedLinkCapacities)
    #Determines link costs based on adjusted link capacities
    LinkFirstDerivativeCosts=AdjustedLinkCapacities/(AdjustedLinkCapacities-LinkRates)^2
    LinkFirstDerivativeCosts[is.nan(LinkFirstDerivativeCosts)]=Inf
    #print("LinkFirstDerivativeCosts");print(LinkFirstDerivativeCosts)
    #Determines second derivative link costs based on adjusted link capacities
    LinkSecondDerivativeCosts=2*AdjustedLinkCapacities/(AdjustedLinkCapacities-LinkRates)^3
    LinkSecondDerivativeCosts[is.nan(LinkSecondDerivativeCosts)]=Inf
    #print("LinkSecondDerivativeCosts");print(LinkSecondDerivativeCosts)
    #Iterates over OD pairs to deviate rate 
    for (i in 1:NumODPairs) {
      Origin=Flows[i,1] #Origin of OD pair
      Destination=Flows[i,2] #Destination of OD pair
      NumPaths=length(Paths[[i]]) #Number of paths of OD pair
      #Determines best path of OD pair
      BestPath=shortestpath(LinkFirstDerivativeCosts,Origin,Destination)
      #Inserts best path in Paths list (if not already there) and determines index
      #of best path within this list; if best path is new its rate is zero
      Best=bestpathinsert(i,NumPaths,Paths,BestPath)
      NumPaths=Best$numroutes; BestPathIndex=Best$bestrouteindex; Paths=Best$paths
      #Determines first derivative path lengths
      PathFirstDerivativeLengths=pathlengths(i,NumPaths,Paths,LinkFirstDerivativeCosts)
      #Deviates rate between routes
      Paths=deviaterate(i,NumPaths,BestPathIndex,Paths,PathFirstDerivativeLengths,LinkSecondDerivativeCosts,Alpha)
      #print("Paths");print(Paths)
    }
    LinkRates=loadlinks(NumNodes,NumODPairs,Paths) #Loads links with rate
    MaxUtilization=max(LinkRates/LinkCapacities,na.rm=TRUE) #Determines maximum link load
    niter1=niter1+1 #Increments number of iterations
  }
  
  #Display number of iterations in phase I
  cat(sprintf("Number of iterations in phase I: %i",niter1),"\n")
  #print("LinkRates");print(LinkRates)
  
  #Phase II: finds optimal solution
  Error2=Inf #Initializes convergence error
  PreviousTotalMeanPackets=0 #Initializes PreviousTotalMeanPackets
  niter2=0 #Initializes number of iterations of phase II
  while (abs(1-Error2)>ErrorPhase2) { #While convergence tolerance not reached
    #Determines link costs
    LinkFirstDerivativeCosts=LinkCapacities/(LinkCapacities-LinkRates)^2
    LinkFirstDerivativeCosts[is.nan(LinkFirstDerivativeCosts)]=Inf
    #print("LinkFirstDerivativeCosts");print(LinkFirstDerivativeCosts)
    #Determines second derivative link costs
    LinkSecondDerivativeCosts=2*LinkCapacities/(LinkCapacities-LinkRates)^3 #Incremental delays based on adjusted link capacities
    LinkSecondDerivativeCosts[is.nan(LinkSecondDerivativeCosts)]=Inf
    #print("LinkSecondDerivativeCosts");print(LinkSecondDerivativeCosts)
    #Iterates over OD pairs to deviate rate
    for (i in 1:NumODPairs) {
      Origin=Flows[i,1] #Origin of OD pair
      Destination=Flows[i,2] #Destination of OD pair
      NumPaths=length(Paths[[i]]) #Number of paths of OD pair
      #Determines best path of OD pair
      BestPath=shortestpath(LinkFirstDerivativeCosts,Origin,Destination)
      #Inserts best path in Paths list (if not already there) and determines index
      #of best path within this list; if best path is new its rate is zero
      Best=bestpathinsert(i,NumPaths,Paths,BestPath)
      NumPaths=Best$numroutes; BestPathIndex=Best$bestrouteindex; Paths=Best$paths
      #Determines first derivative path lengths, of all paths belonging to OD pair i
      PathFirstDerivativeLengths=pathlengths(i,NumPaths,Paths,LinkFirstDerivativeCosts)
      #Deviates rate between routes
      Paths=deviaterate(i,NumPaths,BestPathIndex,Paths,PathFirstDerivativeLengths,LinkSecondDerivativeCosts,Alpha)
      #print("Paths");print(Paths)
    }
    #Loads links with rate
    LinkRates=loadlinks(NumNodes,NumODPairs,Paths)
    #Determines average number of packets in each link
    LinkMeanPackets=LinkRates/(LinkCapacities-LinkRates)
    LinkMeanPackets[which(is.nan(LinkMeanPackets))]=0
    #Determines average number of packets in network
    TotalMeanPackets=sum(LinkMeanPackets)
    #Determines convergence error
    Error2=TotalMeanPackets/PreviousTotalMeanPackets  
    #print("Error2");print(Error2)
    #Updates PreviousTotalMeanPackets
    PreviousTotalMeanPackets=TotalMeanPackets
    #Increments number of iterations
    niter2=niter2+1
  }
  #print("LinkRates");print(LinkRates)
  #print("LinkMeanPackets");print(LinkMeanPackets)
  
  #print("Paths");print(Paths)

  #Print results
  cat(sprintf("Number of iterations in phase 2: %i",niter2),"\n")
  cat("\n")
  cat(sprintf("Average number of packets: %f",TotalMeanPackets),"\n")
  
  cat("\n")
  cat(sprintf("Flow rates:"),"\n")
  for (i in 1:NumODPairs) {
    Origin=Flows[i,1]
    Destination=Flows[i,2]
    cat(sprintf("Flow from %i to %i",Origin,Destination),"\n")
    NumPaths=length(Paths[[i]])
    for (j in 1:NumPaths) {
      ThisPath=Paths[[i]][[j]]$route
      ThisRate=Paths[[i]][[j]]$rate
      cat("  Rate of route ")
      cat(ThisPath,sep="-")
      cat(sprintf(": %f",ThisRate),"\n")
    }
  }
  