script_path <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
if (is.null(script_path) || !nzchar(script_path)) {
  script_arg <- grep("^--file=", commandArgs(), value = TRUE)
  if (length(script_arg)) script_path <- sub("^--file=", "", script_arg[1])
}
if (is.null(script_path) || !nzchar(script_path)) {
  stop("Run this file directly or source it from RStudio so its location can be determined.")
}

script_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
module_dir <- dirname(script_dir)
data_file <- file.path(module_dir, "scripts_helper", "2dtmcdata.txt")
if (!file.exists(data_file)) stop("Attack trace not found: ", data_file)

mydata <- read.table(data_file, sep = "\t")
observations <- as.integer(mydata[[ncol(mydata)]])
if (length(observations) < 2L || anyNA(observations) || any(!observations %in% 0:1)) {
  stop("The attack trace must contain at least two observations, all coded as 0 or 1.")
}

from_state <- head(observations, -1L)
to_state <- tail(observations, -1L)
transition_counts <- table(
  factor(from_state, levels = 0:1),
  factor(to_state, levels = 0:1)
)
if (any(rowSums(transition_counts) == 0L)) {
  stop("Cannot estimate a transition row because the trace never visits its origin state.")
}

transition_probabilities <- prop.table(transition_counts, margin = 1)
results <- data.frame(
  from_state = rep(0:1, each = 2),
  to_state = rep(0:1, times = 2),
  transitions = as.integer(t(transition_counts)),
  probability = as.vector(t(transition_probabilities))
)
stationary_attack_probability <- transition_probabilities[1, 2] /
  (transition_probabilities[1, 2] + transition_probabilities[2, 1])

cat("Number of observations:", length(observations), "\n")
cat("Number of transitions:", length(observations) - 1L, "\n")
cat("Estimated transition matrix (rows: current state; columns: next state):\n")
print(transition_probabilities)
cat("Estimated stationary probability of an attack:",
    round(stationary_attack_probability, 6), "\n")

write.csv(results, file.path(script_dir, "E2_DTMCAttackTransitions.csv"), row.names = FALSE)
