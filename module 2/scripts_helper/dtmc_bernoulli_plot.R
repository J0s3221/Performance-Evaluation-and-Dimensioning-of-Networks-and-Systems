library(ggplot2)
library(gridExtra)

#Input parameters
alpha=0.9
beta=0.1

#2-DTMC transition matrix
P=matrix(c(alpha,1-alpha,beta,1-beta),
         nrow=2,
         ncol=2,
         byrow=TRUE)
#DTMC mean
Avg=P[1,2]/(1+P[2,1]-P[1,1])

Points1=c()
Points2=c()
NumPoints=200

#2-DTMC simulation
CurrentState=0
for (i in 1:NumPoints) {
  p=P[CurrentState+1,CurrentState+1] #Extracts probability of transition to same state
  if (rbinom(1,1,p)==0) { #Changes state if no success
    CurrentState = (CurrentState+1)%%2
  }
  Points1=c(Points1,CurrentState)
}

#Bernoulli process simulation
for (i in 1:NumPoints) {
  Points2=c(Points2,rbinom(1,1,Avg))
}

p1=qplot(y=Points1,ylab="2-DTMC")
p2=qplot(y=Points2,ylab="Bernoulli")
p3=grid.arrange(p1, p2, ncol = 1)
p3

ggsave(p3,file="p3.jpeg",device="jpeg")

