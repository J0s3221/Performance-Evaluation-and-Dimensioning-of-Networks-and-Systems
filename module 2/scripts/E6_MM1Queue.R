# Test for rho = 1 (lambda=1, mu=2), and rho = 2 (lambda=2, mu=1)
# Run 1000, 10000 and 100000 samples to get a clear estimate, without using a warm up period 
# three different values of NumQueueCompleted like requested in the exercise

# M/M/1 simulator: average queueing delay of the first n_max customers served
mm1_delay <- function(ArrivalRate, ServiceRate, n_max) {
    Time=0
    NumQueueCompleted=0
    ServerStatus=0
    NumInQueue=0
    AcumDelay=0
    QueueArrivalTime=c()
    EventList=c(rexp(1,ArrivalRate),Inf)

    while (NumQueueCompleted<n_max) {
        NextEventType=which.min(EventList)
        Time=EventList[NextEventType]
        if (NextEventType==1) {
            EventList[1]=Time+rexp(1,ArrivalRate)
            if (ServerStatus==1) {
                QueueArrivalTime=c(QueueArrivalTime,Time)
                NumInQueue=NumInQueue+1
            } else {
                NumQueueCompleted=NumQueueCompleted+1
                ServerStatus=1
                EventList[2]=Time+rexp(1,ServiceRate)
            }
        } else {
            if (NumInQueue==0) {
                ServerStatus=0
                EventList[2]=Inf
            } else {
                AcumDelay=AcumDelay+Time-QueueArrivalTime[1]
                QueueArrivalTime=QueueArrivalTime[-1]
                NumInQueue=NumInQueue-1
                NumQueueCompleted=NumQueueCompleted+1
                EventList[2]=Time+rexp(1,ServiceRate)
            }
        }
    }

    AvgDelay=AcumDelay/NumQueueCompleted
}

set.seed(1)
R <- 20
n_vals <- c(1e3, 1e4, 1e5)                            # NumQueueCompleted
cases  <- data.frame(rho = c(0.5, 1, 2), lambda = c(1, 1, 2), mu = c(2, 1, 1))

res <- do.call(rbind, lapply(1:nrow(cases), function(i) {
  do.call(rbind, lapply(n_vals, function(n) {
    x <- replicate(R, mm1_delay(cases$lambda[i], cases$mu[i], n))
    data.frame(rho = cases$rho[i], n = n, mean = mean(x),
               hw = qt(0.975, R - 1) * sd(x) / sqrt(R))
  }))
}))
print(res, digits = 4, row.names = FALSE)

# Figure: average delay vs run length (log-log); dotted line = M/M/1 theory for rho = 0.5
jpeg("E6_delay_vs_n.jpeg", width = 900, height = 600, res = 120)
plot(NULL, log = "xy", xlim = range(n_vals), ylim = c(0.3, 4e4),
     xlab = "NumQueueCompleted", ylab = "Average queueing delay")
for (i in seq_along(unique(res$rho))) {
  d <- res[res$rho == unique(res$rho)[i], ]
  lines(d$n, d$mean, type = "b", pch = 15 + i, lwd = 2, col = i)
}
abline(h = 0.5, lty = 3, col = "gray50")
legend("topleft", legend = paste("rho =", unique(res$rho)), col = 1:3, pch = 16:18, lwd = 2)
dev.off()