source("aloha_theo.R")
require(ggplot2)
require(pracma)

N=10
s=logseq(1,1.3,1000)-1
s_size=length(s)


p=0.3
Th1=c(length=s_size)
for (i in 1:s_size) {
  Th1[i]=aloha_theo(N,p,s[i])
}

p=0.4
Th2=c(length=s_size)
for (i in 1:s_size) {
  Th2[i]=aloha_theo(N,p,s[i])
}

p=0.5
Th3=c(length=s_size)
for (i in 1:s_size) {
  Th3[i]=aloha_theo(N,p,s[i])
}

p=0.6
Th4=c(length=s_size)
for (i in 1:s_size) {
  Th4[i]=aloha_theo(N,p,s[i])
}


df=data.frame(x=s,y=c(Th1,Th2,Th3,Th4),p=c(rep("0.3",s_size),rep("0.4",s_size),rep("0.5",s_size),rep("0.6",s_size)))
g=ggplot(df,aes(x=x,y=y,color=p)) + geom_line()
g1=g + scale_x_log10() + xlab("sigma") + ylab("Throughput")
g1

ggsave(g1, file="aloha10.jpeg", device="jpeg")

N=25
s=logseq(1,1.05,1000)-1
s_size=length(s)

p=0.2
Th1=c(length=s_size)
for (i in 1:s_size) {
  Th1[i]=aloha_theo(N,p,s[i])
}

p=0.3
Th2=c(length=s_size)
for (i in 1:s_size) {
  Th2[i]=aloha_theo(N,p,s[i])
}

df=data.frame(x=s,y=c(Th1,Th2),p=c(rep("0.2",s_size),rep("0.3",s_size)))
g=ggplot(df,aes(x=x,y=y,color=p)) + geom_line()
g2=g + scale_x_log10() + xlab("sigma") + ylab("Throughput")
g2

ggsave(g2, file="aloha25.jpeg", device="jpeg")