N=1000
p=0.5
vecB=rbinom(N,1,p)
plot(cumsum(vecB)/c(1:N),type="l")