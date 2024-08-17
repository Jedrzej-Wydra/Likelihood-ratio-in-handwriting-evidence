#LR do wyników

LR<-function(x,y,z)
{
  P0<-pbeta(y,shape1=2.35,shape2=3.63)-pbeta(x,shape1=2.35,shape2=3.63)
  P1<-pbeta(y,shape1=0.31,shape2=1.81)-pbeta(x,shape1=0.31,shape2=1.81)
  LR1<-P0/P1
  LR2<-dbeta(z,shape1=7.93,shape2=12.23)/dbeta(z,shape1=0.31,shape2=1.81)
  r<-data.frame(x=c('P0','P1','LR1','LR2'),y=c(P0,P1,LR1,LR2))
  return(r)
}


#P0/P1

LR(0.03,0.08,0.02)

ggplot(data.frame(x=c( 0,0.5,1),y=c(0,0.5,1)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = 0.31, shape2 = 1.81),colour='#FF9933',size=1)+scale_x_continuous(limits = c(0,5))+scale_y_continuous(limits = c(0,5))
ggplot(data.frame(x=c( 0,0.5,1),y=c(0,0.5,1)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = 2.35, shape2 = 3.63),colour='#FF9933',size=1)+scale_x_continuous(limits = c(0,5))+scale_y_continuous(limits = c(0,5))
