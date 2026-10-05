shortestpath = function (Costs,Source,Destination) {

#Given a matrix of costs this function determines the shorthest path between a
#source and destination using the Dijkstra's algorithm. The route is the
#sequence of nodes from source to destination. Here is an example of a costs
#matrix:
#Costs = [0       10      10      Inf
#         10      0       10      10
#         20      5       0       10
#         Inf     25      5       0];
  
NumNodes=nrow(Costs); #number of nodes
  
NodeLabels=Costs[Source,] #vector of node label

TempNodes=1:NumNodes #vector of temporary nodes
TempNodes=TempNodes[-Source] #remove source node from vector of temporary nodes
TempLabels=NodeLabels #vector of temporary labels
TempLabels=TempLabels[-Source] #remove label of source node from vector of temporary labels

NodeParents=rep(Source,NumNodes) #vector of node parents
  
p=Source #node that becomes permanently labelled
  
while (TRUE) { 
  i=which.min(TempLabels) #determines index of lower label node
  c=min(TempLabels) #determines label of lower label node
  p=TempNodes[i] #p is the next permanent node
  #print(p)
  TempNodes=TempNodes[-i] #remove p node from vector of temporary nodes
  TempLabels=TempLabels[-i] #remove label of p node from vector of temporary labels
  NodeLabels[p]=c #update node label of p node
  if (p==Destination) break #iteration ends when destination found
  for (k in 1:length(TempNodes)) { #update labels of temporary nodes
    nt=TempNodes[k] #next temporary node
    #print(nt)
    ct=NodeLabels[p]+Costs[p,nt] #calculate cost to temporary node via p
    # print(ct)
    # print(Costs[p,nt])
    # print(NodeLabels[nt])
    if (ct < NodeLabels[nt]) { #update only if new cost is lower
      TempLabels[k]=ct #update label on vector of temporary node labels
      NodeLabels[nt]=ct #update label on vector of node labels
      NodeParents[nt]=p #update node parent
    }
  }
}

#determines route from vector of node parents  
Route=c(Destination) 
r=Destination
while (r!=Source) {
  r=NodeParents[r]
  Route=cbind(r,Route);
}

return(Route)

}