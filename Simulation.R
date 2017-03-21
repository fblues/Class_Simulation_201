# # of citizen = 4911500 (4.9 million)

mean(citizen)
length(citizen)

x<-c(0,1)
plot(x,dbinom(x,1,0.6),ylim=c(0,1),xlim=c(-0.5,1.5),col="blue",ylab="probability",lwd=3,type="h",main="Population distribution")

N<-30
m<-500
sam_dist_prop<-function(N=38,p=0.6)
{
  q<-1-p
  citizen<-c(rep(1,floor(4911500*p)),rep(0,floor(4911500*q)))
  sample_proportions<-NULL
  for(i in 1:m)
    sample_proportions[i]<-mean(sample(citizen,N))
  hist(sample_proportions,probability=TRUE,col="yellow",lwd=2,xlim=c(0,1),main="Sampling Distribution of sample proportion")
  curve(dnorm(x,p,sqrt(p*q/N)),add=TRUE,lwd=3)
  abline(v=c(p,mean(sample_proportions)),col=c("Red","Blue"),lwd=3)
  return(c(mean(citizen),  mean(sample_proportions)))
}
sam_dist_prop(500,)





