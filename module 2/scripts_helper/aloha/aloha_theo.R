aloha_theo = function(N,p,s) {

  P=matrix(ncol=N+1,nrow=N+1)

  for (i in 0:N) {
    for (j in 0:N) {
      if (j<i-1) P[i+1,j+1]=0
      if (j==i-1) P[i+1,j+1]=i*p*(1-p)^(i-1)*(1-s)^(N-i)
      if (j==i) P[i+1,j+1]=(1-s)^(N-i)*(1-i*p*(1-p)^(i-1)) + (N-i)*s*(1-s)^(N-i-1)*(1-p)^i
      if (j==i+1) P[i+1,j+1]=(N-i)*s*(1-s)^(N-i-1)*(1-(1-p)^i)
      if (j>i+1) P[i+1,j+1]=choose(N-i,j-i)*s^(j-i)*(1-s)^(N-j)
    }
  }

  E=matrix(rep(1,len=(N+1)^2),nrow=N+1)
  I=diag(N+1)
  e=c(rep(1,N+1))

  pivec=e %*% solve(P+E-I)

  CTh=vector(length=N+1)
  for (i in 0:N) {
    CTh[i+1]=(1-p)^i*(N-i)*s*(1-s)^(N-i-1)+i*p*(1-p)^(i-1)*(1-s)^(N-i)
  }

  Th_theo=sum(pivec*CTh)
  
  return(Th_theo)
  
}