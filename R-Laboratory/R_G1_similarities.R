library(readxl)
library(data.table)
library("OmicsMarkeR", lib.loc="D:/R/R-3.5.3/library")
library(ggplot2)
sheetnames<-excel_sheets(path)
dane <- lapply(excel_sheets(path), read_excel, path = path)
names(dane) <- sheetnames
dane<-lapply(dane,data.frame)
dane2<-dane
ori<-c('ori','a','a','a','a','a')
dod<-function(x)
{
  x<-rbind(x,ori)
}
dane<-lapply(dane,dod)
rm<-dane[[1]]$falsifier
macierz <- function(x)
{
  rm<-x[,1]
  x[,1]<-NULL
  x<-as.matrix(x)
  x<-sapply(x,use.switch)
  x<-matrix(x,30,5)
}
dane<-lapply(dane,macierz)
dane<-lapply(dane,data.frame)
for(i in 1:22)
{
  rownames(dane[[i]])<-rm
  names(dane[[i]])<-c('C1','C2','C3','C4','C5')
}
for(i in 1:22)
{
  dane[[i]][,1]<-dane[[i]][,1]+0.1
  dane[[i]][,2]<-dane[[i]][,2]+0.2
  dane[[i]][,3]<-dane[[i]][,3]+0.3
  dane[[i]][,4]<-dane[[i]][,4]+0.4
  dane[[i]][,5]<-dane[[i]][,5]+0.5
}
for(i in 1:22)
{
  dane[[i]]<-sapply(dane[[i]],factor)
}

smx<-function(x)
{
  a=x
  for(i in 2:30)
  {a=rbind(a,x)}
  b=t(matrix(rep(x[1,],30),5,30))
  for(i in 2:30)
  {
    b=rbind(b,t(matrix(rep(x[i,],30),5,30)))
  }
  mx=cbind(a,b)
  return(mx)
}
pod<-function(x)
{
  x=smx(x)
  a=jaccard(x[1,1:5],x[1,6:10])
  for(i in 2:900)
  {
    a=c(a,jaccard(x[i,1:5],x[i,6:10]))
  }
  x=matrix(a,30,30)
}
dane3<-lapply(dane,pod)
prze<-function(x)
{
  x<-melt(x,varnames=c('y','z'),value.name='similarity')
}
dane4<-lapply(dane3,prze)
for(i in 1:22)
{
  dane4[[i]]$numer<-rep(i,30)
}
wyk<-dane4[[1]]
for(i in 2:22)
{
  wyk<-rbind(wyk,dane4[[i]])
}
ggplot(wyk,aes(x=y,y=z))+geom_tile(aes(fill=similarity),colour="#000000")+scale_fill_gradient2(low="#3399FF",mid="#FFCC33",high="#CC0000",guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(0,1),midpoint=0.5)+theme_minimal()+labs(x=NULL,y=NULL,fill='similarity')+facet_wrap(~numer)
zak<-function(x)
{
  x<-round(x,digits = 3)
}
dane5<-lapply(dane3,zak)
usu<-function(x)
{
  x<-x[30,]
}
dane5<-lapply(dane5,usu)
usu2<-function(x)
{
  x<-x[1:29]
}
dane5<-lapply(dane5,usu2)
dane8<-dane5
dane5<-lapply(dane5,table)
for(i in 1:22)
{
  dane5[[i]]<-data.frame(value=names(dane5[[i]]),similarity=dane5[[i]],nr=rep(i,length(dane5[[i]])))
}
for(i in 1:22)
{
  dane5[[i]]<-dane5[[i]][,-2]
  names(dane5[[i]])<-c('value','similarity','nr')
}
wyk2<-dane5[[1]]
for(i in 2:22)
{
  wyk2<-rbind(wyk2,dane5[[i]])
}
wyk2$nr<-factor(wyk2$nr)
wyk2<-data.table(wyk2)
ggplot(wyk2,aes(x=value,y=similarity))+geom_col(aes(fill=nr))+facet_wrap(~nr)+theme(legend.position = 'none')+labs(x='similarity',y='count')
ggplot(data.frame(x=c(0,0.5,1),y=c(0,0.5,1)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = 1, shape2 = 3),colour='#66CC00',size=1)
ggplot(data.frame(x=c(0,0.5,1),y=c(0,0.5,1)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = 1, shape2 = 3),colour='#66CC00',size=1)+scale_x_continuous(limits = c(0,3))+scale_y_continuous(limits = c(0,3))
use.switch2<-function(x)
{
  switch(x,
         '0'=0,
         '0.111'=0.111,
         '0.25'=0.25,
         '0.429'=0.429,
         '0.667'=0.667,
         '1'=1)
}
zmiana<-function(x)
{
  x$value<-sapply(x,use.switch2)
}
for(i in 1:22)
{
  dane5[[i]]$value<-sapply(dane5[[i]]$value,use.switch2)
}
cumulative<-function(x)
{
  a<-length(x[,2])
  while(a>0)
  {
    x[a,2]<-sum(x[1:a,2])
    a<-a-1
  }
  return(x)
}
dane6<-dane5
dane6<-lapply(dane5,cumulative)

dane5[[1]][4:6,]<-data.frame(c(0.429,0.667,1.000),c(0,0,0),c(1,1,1))
dane5[[2]][6,]<-c(1.000,0,2)
dane5[[3]][6,]<-c(1.000,0,3)
dane5[[4]][6,]<-c(1.000,0,4)
dane5[[5]][6,]<-c(1.000,0,5)
dane5[[7]][6,]<-c(1.000,0,7)
dane5[[8]][6,]<-c(1.000,0,8)
dane5[[9]][6,]<-c(1.000,0,9)
dane5[[11]][4,]<-dane5[[11]][3,]
dane5[[11]][3,]<-c(.250,0,11)
dane5[[11]][5:6,]<-data.frame(c(0.667,1.000),c(0,0),c(11,11))
dane5[[12]][6,]<-dane5[[12]][5,]
dane5[[12]][5,]<-c(0.667,0,12)
dane5[[13]][6,]<-c(1.000,0,13)
dane5[[14]][6,]<-c(1.000,0,14)
dane5[[15]][6,]<-c(1.000,0,15)
dane5[[16]][5:6,]<-data.frame(c(0.667,1.000),c(0,0),c(16,16))
dane5[[17]][6,]<-c(1.000,0,17)
dane5[[20]][4:6,]<-data.frame(c(0.429,0.667,1.000),c(0,0,0),c(20,20,20))
dane5[[21]][6,]<-dane5[[21]][5,]
dane5[[21]][5,]<-c(0.667,0,21)
dane5[[22]][5:6,]<-data.frame(c(0.667,1.000),c(0,0),c(22,22))

dane5[[11]][4,]<-c(0.429,2,11)
dane5[[12]][6,]<-c(1.000,1,12)
dane5[[21]][6,]<-c(1.00,1,21)

p1<-pbeta(0.2,shape1 = 1,shape2 = 3)
p2<-pbeta(0.4,shape1 = 1,shape2 = 3)-pbeta(0.2,shape1 = 1,shape2 = 3)
p3<-pbeta(0.6,shape1 = 1,shape2 = 3)-pbeta(0.4,shape1 = 1,shape2 = 3)
p4<-pbeta(0.8,shape1 = 1,shape2 = 3)-pbeta(0.6,shape1 = 1,shape2 = 3)
p5<-pbeta(1,shape1 = 1,shape2 = 3)-pbeta(0.8,shape1 = 1,shape2 = 3)
prawdo<-c(p1,p2,p3,p4,p5)
chi<-function(x)
{
  a<-x[,2]
  x<-chisq.test(c(sum(a[1:2]),a[3:6]),p=prawdo)$p.value
}
dane7<-lapply(dane5,chi)
wyk3<-dane7[[1]]
for(i in 2:22)
{
  wyk3<-c(wyk3,dane7[[i]])
}
wyk3<-data.frame(podpis=c(1:22),pvar=wyk3)
ggplot(wyk3,aes(x=podpis,y=pvar))+geom_col(aes(fill=podpis))+geom_line(y=0.01,size=1,colour='#660000')+theme(legend.position = "none")+labs(x='podpis',y='p-warto??')
dane9<-lapply(dane8,mean)
dane9.2<-lapply(dane8,var)
ab<-function(x,y)
{
  alpha<-(((x^2)-(x^3))/y)-x
  beta<-((x-2*(x^2)+(x^3))/y)+x-1
  return(c(shape1=alpha,shape2=beta))
}
momenty<-c(x=dane9[[1]],y=dane9.2[[1]])
for(i in 2:22)
{
  k<-c(dane9[[i]],dane9.2[[i]])
  momenty<-rbind(momenty,k)
}
aabb<-ab(momenty[1,1],momenty[1,2])
for(i in 2:22)
{
  aabb<-rbind(aabb,ab(momenty[i,1],momenty[i,2]))
}
mean(aabb[,1])
mean(aabb[,2])

p1.2<-pbeta(0.2,shape1 = 0.43,shape2 = 2.67)
p2.2<-pbeta(0.4,shape1 = 0.43,shape2 = 2.67)-pbeta(0.2,shape1 = 0.43,shape2 = 2.67)
p3.2<-pbeta(0.6,shape1 = 0.43,shape2 = 2.67)-pbeta(0.4,shape1 = 0.43,shape2 = 2.67)
p4.2<-pbeta(0.8,shape1 = 0.43,shape2 = 2.67)-pbeta(0.6,shape1 = 0.43,shape2 = 2.67)
p5.2<-pbeta(1,shape1 = 0.43,shape2 = 2.67)-pbeta(0.8,shape1 = 0.43,shape2 = 2.67)
prawdo2<-c(p1.2,p2.2,p3.2,p4.2,p5.2)
chi<-function(x)
{
  a<-x[,2]
  x<-chisq.test(c(sum(a[1:2]),a[3:6]),p=prawdo2)$p.value
}
dane10<-lapply(dane5,chi)
wyk4<-dane10[[1]]
for(i in 2:22)
{
  wyk4<-c(wyk4,dane10[[i]])
}
wyk4<-data.frame(podpis=c(1:22),pvar=wyk4)
ggplot(wyk4,aes(x=podpis,y=pvar))+geom_col(aes(fill=podpis))+geom_line(y=0.01,size=1,colour='#660000')+theme(legend.position = "none")+labs(x='podpis',y='p-warto??')
ggplot(data.frame(x=c(0,0.5,1),y=c(0,0.5,1)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = 0.43, shape2 = 2.67),colour='#FF33FF',size=1)
ggplot(data.frame(x=c(0,0.5,1),y=c(0,0.5,1)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = 0.43, shape2 = 2.67),colour='#FF33FF',size=1)+scale_x_continuous(limits = c(0,5))+scale_y_continuous(limits = c(0,5))

dane10<-dane3
for(i in 1:22)
{
  dane10[[i]]<-data.frame(dane3[[i]][30,1:29],rep(i,29))
}

wyk5<-dane10[[1]]
for(i in 2:22)
{
  wyk5<-rbind(wyk5,dane10[[i]])
}
names(wyk5)<-c('value','nr')
wyk5$nr2<-wyk5$nr
wyk5$nr2<-factor(wyk5$nr2)
ggplot(wyk5,aes(x=value))+stat_bin(breaks=c(0,0.2,0.4,0.6,0.8,1),aes(fill=nr2))+facet_wrap(~nr2)+theme(legend.position = 'none')+labs(x='similarity',y='count')+scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))

ggplot(data.frame(x=c(0,1.5,3),y=c(0,1.5,3)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = 0.43, shape2 = 2.67),colour='#FF33FF',size=1)+stat_function(fun = dbeta, args = list(shape1 = 1, shape2 = 3),colour='#66CC00',size=1)+scale_x_continuous(limits = c(0,5))+scale_y_continuous(limits = c(0,5))

ggplot(wyk5,aes(x=value))+stat_bin(breaks=c(0,0.2,0.4,0.6,0.8,1),aes(fill=nr2))+theme(legend.position = 'none')+labs(x='similarity',y='count')+scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))

j<-wyk5

p1<-pbeta(0.2,shape1 = ab(j)[1],shape2 = ab(j)[2])
p2<-pbeta(0.4,shape1 = ab(j)[1],shape2 = ab(j)[2])-pbeta(0.2,shape1 = ab(j)[1],shape2 = ab(j)[2])
p3<-pbeta(0.6,shape1 = ab(j)[1],shape2 = ab(j)[2])-pbeta(0.4,shape1 = ab(j)[1],shape2 = ab(j)[2])
p4<-pbeta(0.8,shape1 = ab(j)[1],shape2 = ab(j)[2])-pbeta(0.6,shape1 = ab(j)[1],shape2 = ab(j)[2])
p5<-pbeta(1,shape1 = ab(j)[1],shape2 = ab(j)[2])-pbeta(0.8,shape1 = ab(j)[1],shape2 = ab(j)[2])
prawdo<-c(p1,p2,p3,p4,p5)

j<-wyk5
ggplot(data.frame(x=c(0,1.5,3),y=c(0,1.5,3)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = ab(j)[1], shape2 = ab(j)[2]),colour='#330066',size=1)+stat_function(fun = dbeta, args = list(shape1 = 0.43, shape2 = 2.67),colour='#FF33FF',size=1)+scale_x_continuous(limits = c(0,5))+scale_y_continuous(limits = c(0,5))

ggplot(wyk,aes(x=similarity))+stat_bin(breaks=c(0,0.2,0.4,0.6,0.8,1),aes(fill=y))+facet_wrap(~y)+theme(legend.position = 'none')+labs(x='similarity',y='count')+scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))

ab(wyk[,3])
ob<-data.table(wyk)
ob2<-subset(ob,y!=z&y!=30&z!=30)
ob3<-subset(ob,y!=z)
ab(as.matrix(ob2[,3]))
ab(as.matrix(ob3[,3]))

ggplot(data.frame(x=c(0,0.5,1),y=c(0,0.5,1)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = 0.3199256, shape2 = 4.3683450),colour='#00CCCC',size=1)
ggplot(data.frame(x=c(0,0.5,1),y=c(0,0.5,1)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = 0.2973492, shape2 = 3.7449014),colour='#00CC00',size=1)+stat_function(fun = dbeta, args = list(shape1 = 0.3199256, shape2 = 4.3683450),colour='#00CCCC',size=1)+stat_function(fun = dbeta, args = list(shape1 = 0.3146313, shape2 = 1.8105278),colour='#00CCCC',size=1)

ggplot(wyk,aes(x=y,y=z))+geom_tile(aes(fill=similarity),colour="#CCCCCC")+scale_fill_gradient2(low="#FFFFFF",mid="#999999",high="#000000",guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(0,1),midpoint=0.5)+theme_minimal()+labs(x=NULL,y=NULL,fill='similarity')+facet_wrap(~numer,ncol=4)

ggplot(wyk5,aes(x=value))+stat_bin(breaks=c(0,0.2,0.4,0.6,0.8,1),fill='#9966FF')+facet_wrap(~nr2,ncol=3)+theme(legend.position = 'none')+labs(x='similarity',y='count')+scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_classic()+theme(text = element_text(size=20))+xlab('podobie?stwo')+ylab('liczno??')

ggplot(wyk5,aes(x=value))+stat_bin(breaks=c(0,0.2,0.4,0.6,0.8,1),fill='#9966FF')+theme(legend.position = 'none')+labs(x='similarity',y='count')+scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_classic()+xlab('podobie?stwo')+ylab('liczno??')



ggplot(wyk,aes(x=y,y=z))+geom_tile(aes(fill=similarity),colour="#3399FF")+scale_fill_gradient2(low="#3399FF",mid="#FFCC33",high="#CC0000",guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(0,1),midpoint=0.5)+theme_minimal()+labs(x=NULL,y=NULL,fill='podobie?stwo')+facet_wrap(~numer,ncol=4)+theme(text = element_text(size=20))
ggplot(data.frame(x=c(0,1.5,3),y=c(0,1.5,3)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = ab(j$value)[1], shape2 = ab(j$value)[2]),colour='#33CCCC',size=1)+scale_x_continuous(limits = c(0,1))+scale_y_continuous(limits = c(0,4))+theme_classic()


ggplot(wyk,aes(x=y,y=z))+geom_tile(aes(fill=similarity),colour="#CCCCCC")+scale_fill_gradient2(low="#FFFFFF",mid="#999999",high="#000000",guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(0,1),midpoint=0.5)+theme_minimal()+labs(x=NULL,y=NULL,fill='podobie?stwo')+facet_wrap(~numer,ncol=4)

x<-c(0,0,0.2,0.2,0.4,0.4,0.6,0.6,0.8,0.8,1,1)
y<-c(0,7.2,7.2,1.616,1.616,0.8,0.8,0.432,0.432,0.16,0.16,0)
y<-y*5/7.2
h=data.frame(x,y)

ggplot(h,aes(x,y))+geom_polygon(fill='#9966FF')+stat_function(fun = dbeta, args = list(shape1 = 0.31, shape2 = 1.81),colour='#000033',size=1.06)+scale_y_continuous(limits=c(0,6),breaks = c(0,1,2,3,4,5))+theme_classic()


ggplot(wyk5,aes(x=value))+stat_bin(breaks=c(0,0.2,0.4,0.6,0.8,1),fill='#9966FF')+theme(legend.position = 'none')+labs(x='similarity',y='count')+scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_classic()+xlab('Similarity between original and simulated signatures')+ylab('Number of simulated signatures')
ggplot(wyk5,aes(x=value))+stat_bin(breaks=c(0,0.2,0.4,0.6,0.8,1),fill='#9966FF')+facet_wrap(~nr2,ncol=3)+theme(legend.position = 'none')+labs(x='similarity',y='count')+scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_classic()+theme(text = element_text(size=20))+xlab('Similarity between original and simulated signatures')+ylab('Number of simulated signatures')

