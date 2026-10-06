# Path to the module folder
module_dir <- "/Users/tiagovideira/OneDrive - tecnico.pt/Performance-Evaluation-and-Dimensioning-of-Networks-and-Systems/module 2"

# Read the attack trace (the last column holds the 0/1 observations)
mydata <- read.table(file.path(module_dir, "scripts_helper", "2dtmcdata.txt"), sep = "\t")
observations <- mydata[[ncol(mydata)]]

# Pair each observation with the one that follows it
from_state <- head(observations, -1)
to_state   <- tail(observations, -1)

# Count the transitions, then turn each row into probabilities
transition_counts        <- table(from_state, to_state)
transition_probabilities <- prop.table(transition_counts, margin = 1)

# Long-run (stationary) probability of being in the attack state
p01 <- transition_probabilities["0", "1"]
p10 <- transition_probabilities["1", "0"]


# Print results
cat("Number of observations:", length(observations), "\n")
print(transition_probabilities)

# Save the transition probabilities to a CSV
write.csv(as.data.frame(transition_probabilities),
          file.path(module_dir, "scripts", "E2_DTMCAttackTransitions.csv"),
          row.names = FALSE)