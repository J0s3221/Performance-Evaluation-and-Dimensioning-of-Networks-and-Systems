#This routine calculates and prints the performance metrics

report = function()

for (i in 1:nrow(FlowStats)) {
  cat(sprintf("Average delay in flow %d = %f", i, FlowStats[i,2]/FlowStats[i,1]),"\n")
  cat(sprintf("Flow throughput in flow %d = %f", i, FlowStats[i,3]/Time),"\n")
}