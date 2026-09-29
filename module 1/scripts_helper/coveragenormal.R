#Estimates the coverage of confidence intervals

E=1000 #Number of experiments
SizesOfSamples=c(5,10,50,5000) #Sizes of samples
tcritical=c(qt(0.975,df=SizesOfSamples[1]-1),
            qt(0.975,df=SizesOfSamples[2]-1),
            qt(0.975,df=SizesOfSamples[3]-1),
            qt(0.975,df=SizesOfSamples[4]-1)) #Critical points t distribution

#Standard normal distribution
mu=0
InInt=rep(0,4)
for (i in 1:4) {
  N=SizesOfSamples[i]
  for (j in 1:E) {
    a=rnorm(N)
    ma=mean(a)
    va=var(a)
    alinf=ma-tcritical[i]*sqrt(va/N)
    alsup=ma+tcritical[i]*sqrt(va/N)
    if (mu>alinf & mu<alsup) InInt[i]=InInt[i]+1
  }
}
CICoverage=InInt/E
print("Standard Normal distribution")
print(CICoverage)

#Exponential distribution

#Standard Lognormal distribution with sigma^2=1

#Standard Lognormal distribution with sigma^2=2
