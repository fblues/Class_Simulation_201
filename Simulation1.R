#pop=uniform
m<-500
x<-1:10000/500

plot(x,dexp(x,1/9),lwd=3,type="l",main="Population distribution: Highly Skewed Version")

N<-30
sam_dist_mean_exp<-function(N=30)
{
sample_means<-NULL
for(i in 1:m)
  sample_means[i]<-mean(rexp(N,1/9))
hist(sample_means,probability=TRUE,lwd=2,col="yellow",main="Sampling Distribution of sample mean",xlim=c(0,20))
abline(v=c(9,mean(sample_means)),col=c("Red","Blue"),lwd=3)
curve(dnorm(x,9,9/sqrt(N)),add=TRUE,lwd=3)
}

sam_dist_mean_exp(4)



######Uniform (0,18)
plot(x,dunif(x,0.1,18),lwd=3,xlim=c(0,18),type="l",main="Population distribution: Uniform Version")



N<-30
sam_dist_mean_unif<-function(N=30)
{
sample_means<-NULL
for(i in 1:m)
  sample_means[i]<-mean(runif(N,0,18))
hist(sample_means,probability=TRUE,xlim=c(2,16),lwd=2,col="yellow",main="Sampling Distribution of sample mean")
abline(v=c(9,mean(sample_means)),col=c("Red","Blue"),lwd=3)
curve(dnorm(x,9,3*sqrt(3)/sqrt(N)),add=TRUE,lwd=3)
}
sam_dist_mean_unif(30)

#####################
#normal case
plot(x,dnorm(x,9,3),lwd=3,xlim=c(0,18),type="l",main="Population distribution: Normal Version")


sam_dist_mean_norm<-function(N=30)
{
sample_means<-NULL
for(i in 1:m)
  sample_means[i]<-mean(rnorm(N,9,3))
hist(sample_means,probability=TRUE,xlim=c(2,16),col="yellow",lwd=2,main="Sampling Distribution of sample mean")
abline(v=c(9,mean(sample_means)),col=c("Red","Blue"),lwd=3)
curve(dnorm(x,9,3/sqrt(N)),add=TRUE,lwd=3)
}
sam_dist_mean_norm(1000)












