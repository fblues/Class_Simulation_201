LLN<-function(x)
{
  n<-length(x)
  p<-NULL
  for(i in 1:n)
    p[i]<-mean(x[1:i])
  print(plot(1:n,p,type="l",col="Blue",lwd=3,main="STAT201 L.L.N. Simulation",ylab="Probability",xlab="# of Coins"))
  print(abline(h=1/2,col="Red",lwd=3))
  print(p[n])
}


Simul<-function(y)
{
  for(i in 1:72)
  {
    z<-scan(n=1)
    y<-c(y,z)
    LLN(y)
  }
}
Simul(0)
