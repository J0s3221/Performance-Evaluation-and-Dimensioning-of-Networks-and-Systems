#This function calculates the blocking probability of each flow and ouputs the
#results

report = function() {
  
  for (i in 1:nrow(FlowStats)) {
    cat(sprintf("Blocking probability of flow %d = %f",i,FlowStats[i,2]/FlowStats[i,1]),"\n")
  }
  
}