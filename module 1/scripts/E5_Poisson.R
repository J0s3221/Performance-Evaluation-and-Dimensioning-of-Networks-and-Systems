lambda <- 2.0         
n_arrivals <- 10000

inter_arrival_times <- rexp(n_arrivals, rate = lambda)

arrival_times <- cumsum(inter_arrival_times)

estimated_lambda <- 1/mean(inter_arrival_times)

cat("Taxa da população (lambda):", lambda, "\n")
cat("Taxa estimada (amostral):", estimated_lambda, "\n")

head(arrival_times, 10)