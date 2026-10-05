loadlinks = function(NumNodes,NumODPairs,Paths) {
#Loads links with rate contained in Paths list
  LinkRates=matrix(rep(0,NumNodes^2),nrow=NumNodes) #Initializes LinkRates matrix with all zeros
  for (i in 1:NumODPairs) { #Iterates over OD pairs
    NumPaths=length(Paths[[i]]) #Number of paths in OD pair
    for (j in 1:NumPaths) { #Iterates over paths of OD pair
      ThisPath=Paths[[i]][[j]]$route #Current path
      ThisRate=Paths[[i]][[j]]$rate #Current rate
      for (k in 1:(length(ThisPath)-1)) { #Iterates over links of path
        LinkOrigin=ThisPath[k] #Origin of link
        LinkDestination=ThisPath[k+1] #Destination of link
        #Loads link with rate of path
        LinkRates[LinkOrigin,LinkDestination]=LinkRates[LinkOrigin,LinkDestination]+ThisRate 
      }
    }
  }
  return(LinkRates)
}

bestpathinsert = function(ODPair,NumPaths,Paths,BestPath) {
#Inserts best path in Paths list for specific OD pair (if not already there) and determines index of
#best path within this list; if best path is new its rate is zero
  BestPathIndex=0 #Initializes index of best path
  for (j in 1:NumPaths) { #Iterates over paths
    ThisPath=Paths[[ODPair]][[j]]$route #Extracts path from Paths list
    if (length(BestPath)==length(ThisPath) && all(BestPath==ThisPath)) { #Best path is already in list
      BestPathIndex=j #Sets index of best path
      break
    }
  }
  if (BestPathIndex==0){ #Best path is not in Paths list
    NumPaths=NumPaths+1 #Increments number of path
    BestPathIndex=NumPaths #Sets index of best path
    Paths[[ODPair]][[BestPathIndex]]=list(route=BestPath,rate=0) #Inserts best path in Paths list
  }
  return(list(numroutes=NumPaths,bestrouteindex=BestPathIndex,paths=Paths))
}

pathlengths = function(ODPair,NumPaths,Paths,LinkFirstDerivativeCosts) {
#Determines first derivative path lengths of all paths belonging to given OD pair
  PathFirstDerivativeLengths=vector(mode="numeric",length=NumPaths) #Initializes PathFirstDerivativeLengths vector
  for (j in 1:NumPaths) { #Iterates over paths
    ThisPath=Paths[[ODPair]][[j]]$route #Extracts path from Paths list
    ThisLength=0 #Initializes path length
    for (k in 1:(length(ThisPath)-1)) { #Iterates over links of path
      LinkOrigin=ThisPath[k] #Origin of link
      LinkDestination=ThisPath[k+1] #Destination of link
      ThisLength=ThisLength+LinkFirstDerivativeCosts[LinkOrigin,LinkDestination] #Updates path length
    }
    PathFirstDerivativeLengths[j]=ThisLength #Stores path length in PathFirstDerivativeLengths vector
  }
  return(PathFirstDerivativeLengths)
}
  
deviaterate = function(ODPair,NumPaths,BestPathIndex,Paths,PathFirstDerivativeLengths,LinkSecondDerivativeCosts,Alpha) {
#Deviates rate between paths of given OD pair, knowing the best route
  
  #Extracts MFDL and the path that owns it
  MFDL=PathFirstDerivativeLengths[BestPathIndex] #Extracts MFDL
  #print("BestIncDelayFirst");print(BestIncDelayFirst)
  BestPath=Paths[[ODPair]][[BestPathIndex]]$route #Extracts path with MFDL
  for (i in 1:NumPaths) { #Iterates over paths
    if (BestPathIndex==i) { #Path is MFDL
      #Do noting
    } else { #Path is non-MFDL
      ThisLength=PathFirstDerivativeLengths[i]
      #print("ThisLength");print(ThisLength)
      #Calculates Hp parameter
      ThisPath=Paths[[ODPair]][[i]]$route #Extracts path from Paths list
      NonRepeatedLinks=nonrepeatedlinks(ThisPath,BestPath) #Determines list of links not common to MFDL path and non-MFDL path
      Hp=0 #Initializes Hp parameter
      for (j in 1:length(NonRepeatedLinks)) { #Iterates over non-repeted links
        LinkOrigin=NonRepeatedLinks[[j]][1] #Origin of link
        LinkDestination=NonRepeatedLinks[[j]][2] #Destination of link
        Hp=Hp+LinkSecondDerivativeCosts[LinkOrigin,LinkDestination] #Updates Hp parameter
      }
      #print("Hp");print(Hp)
      #Calculates Delta parameter
      Delta=Alpha*(ThisLength-MFDL)/Hp
      #print("Delta");print(Delta)
      #Deviates rate
      PreviousRate=Paths[[ODPair]][[i]]$rate
      NewRate=PreviousRate-Delta #Checks if new rate of non-MFDL path is non-negative
      if (NewRate>=0) {
        Paths[[ODPair]][[BestPathIndex]]$rate=Paths[[ODPair]][[BestPathIndex]]$rate+Delta #Adds Delta to MFDL path
        Paths[[ODPair]][[i]]$rate=NewRate #Subtracts Delta from non-MFDL path
      } else {
        Paths[[ODPair]][[i]]$rate=0
        Paths[[ODPair]][[BestPathIndex]]$rate=Paths[[ODPair]][[BestPathIndex]]$rate+PreviousRate
      }
    }
  }
  return(Paths)
}

nonrepeatedlinks = function(Path1,Path2) {
#Returns list with the links that are not common to Path1 and Path2
  NonRepeatedLinks=list() #Initializes NonRepeatedLinks list
  for (j in 1:(length(Path1)-1)) { #Iterates over links of Path1
    NonRepeatedLinks[[j]]=Path1[j:(j+1)] #Loads NonRepeatedLinks list with links of Path1
  }
  for (j in 1:(length(Path2)-1)) { #Iterates over links of Path2
    ThisLink=Path2[j:(j+1)] #Extracts link from Path2
    RepeatedLink=FALSE #Set ThisLink as not repeated, initially
    for (k in 1:length(NonRepeatedLinks)) { #Iterates over links already in NonRepeatedLinks list
      if (identical(NonRepeatedLinks[[k]],ThisLink)) { #If ThisLink is already in NonRepeatedLinks list
        RepeatedLink=TRUE #ThisLink is repeated
        break #Stop searching
      }
    }
    if (RepeatedLink==FALSE) { #If ThisLink is not repeated
      NonRepeatedLinks[[length(NonRepeatedLinks)+1]]=ThisLink #Insert ThisLink in NonRepeatedLinks list
    }
  }
  return(NonRepeatedLinks)
}