#This function calculates the average delay of each flow and outputs the results

report = function() {
  
  for (i in 1:numFlows) {
    cat(sprintf("Average delay of flow %d = %f",i,FlowStats[i,2]/FlowStats[i,1]),"\n")
  }
}