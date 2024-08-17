Bayes<-function(x,y,z)
{
  j<-pbeta(y,ab(z)[1],ab(z)[2])-pbeta(x,ab(z)[1],ab(z)[2])
  k<-pbeta(y,0.5,2.7)-pbeta(x,0.5,2.7)
  l<-j/k
  return(data.frame(H0=j,H1=k,LR=l))
}

ufn<-function(x)
{
  a<-mean(x)-qnorm(0.975)*(sd(x)/sqrt(length(x)))
  b<-mean(x)+qnorm(0.975)*(sd(x)/sqrt(length(x)))
  return(c(a,b))
}

ufn2<-function(x)
{
  a<-mean(x)-qt(0.95,length(x)-1)*(sd(x)/sqrt(length(x)))
  b<-mean(x)+qt(0.95,length(x)-1)*(sd(x)/sqrt(length(x)))
  return(c(a,b))
}