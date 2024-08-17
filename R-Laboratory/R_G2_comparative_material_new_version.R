sheetnames<-excel_sheets(path)
dane <- lapply(excel_sheets(path), read_excel, path = path)
names(dane) <- sheetnames
dane<-lapply(dane,data.frame)
dane[[2]]<-dane[[2]][-38,]
for(i in 1:3)
{
  dane[[i]]<-dane[[i]][,-1]
  dane[[i]]<-dane[[i]][,-8]
  dane[[i]]<-dane[[i]][,-7]
  dane[[i]]<-dane[[i]][,-6]
}
dane2<-dane
d<-NULL
for(i in 1:3)
{
  a<-dane[[i]]
  a<-as.matrix(a)
  b<-a
  for(j in 2:dim(a)[1])
  {
    b<-rbind(b,a)
  }
  f<-NULL
  for(k in 1:dim(a)[1])
  {
    e<-NULL
    for(l in 1:dim(a)[1])
    {
      e<-rbind(e,a[k,])
    }
    f<-rbind(f,e)
  }
  dane[[i]]<-cbind(b,f)
}
dane3<-dane
for(i in 1:3)
{
  a<-dane[[i]]
  b<-sqrt(dim(a)[1])
  f<-jaccard(a[1,1:5],a[1,6:10])
  for(j in 2:dim(a)[1])
  {
    f<-c(f,jaccard(a[j,1:5],a[j,6:10]))
  }
  dane[[i]]<-data.frame(matrix(f,b))
  names(dane[[i]])<-c(1:b)
  rownames(dane[[i]])<-c(1:b)
  dane3[[i]]<-melt(dane[[i]], varnames=c('x','y'),value.name="similarity")
  x<-rep(1:b,b)
  dane3[[i]]<-data.frame(x,dane3[[i]])
  names(dane3[[i]])<-c('x','y','similarity')
}
ggplot(dane3[[1]],aes(x=similarity))+stat_bin(breaks = c(0,0.2,0.4,0.6,0.8,1))
dane4<-list(data.table(dane3[[1]]),data.table(dane3[[2]]),data.table(dane3[[3]]))
ggplot(dane3[[1]],aes(x=x,y=y))+geom_tile(aes(fill=similarity),colour="#000000")+scale_fill_gradient2(low="#3399FF",mid="#FFCC33",high="#CC0000",guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(0,1),midpoint=0.5)+theme_minimal()+labs(x=NULL,y=NULL,fill='similarity')+scale_x_discrete(limits=c(1:49))
ggplot(dane3[[2]],aes(x=x,y=y))+geom_tile(aes(fill=similarity),colour="#000000")+scale_fill_gradient2(low="#3399FF",mid="#FFCC33",high="#CC0000",guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(0,1),midpoint=0.5)+theme_minimal()+labs(x=NULL,y=NULL,fill='similarity')+scale_x_discrete(limits=c(1:37))
ggplot(dane3[[3]],aes(x=x,y=y))+geom_tile(aes(fill=similarity),colour="#000000")+scale_fill_gradient2(low="#3399FF",mid="#FFCC33",high="#CC0000",guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(0,1),midpoint=0.5)+theme_minimal()+labs(x=NULL,y=NULL,fill='similarity')+scale_x_discrete(limits=c(1:47))

dane4[[1]]<-subset(dane4[[1]],x!=y&y!=49&x!=49)
dane4[[2]]<-subset(dane4[[2]],x!=y&y!=37&x!=37)
dane4[[3]]<-subset(dane4[[3]],x!=y&y!=47&x!=47)
ggplot(dane4[[1]],aes(x=similarity))+stat_bin(fill='#669900',breaks = c(0,0.2,0.4,0.6,0.8,1))
ggplot(dane4[[2]],aes(x=similarity))+stat_bin(fill='#006666',breaks = c(0,0.2,0.4,0.6,0.8,1))
ggplot(dane4[[3]],aes(x=similarity))+stat_bin(fill='#990099',breaks = c(0,0.2,0.4,0.6,0.8,1))

a<-dane4[[1]][,3]
b<-dane4[[2]][,3]
c<-dane4[[3]][,3]
a<-as.matrix(a)
b<-as.matrix(b)
c<-as.matrix(c)
at<-table(cut(a,breaks = 5))
bt<-table(cut(b,breaks = 5))
ct<-table(cut(c,breaks = 5))
at<-c(at)
bt<-c(bt)
ct<-c(ct)
ab<-function(x)
{
  m<-mean(x)
  v<-var(x)
  alpha<-(((m^2)-(m^3))/v)-m
  beta<-((m-2*(m^2)+(m^3))/v)+m-1
  return(c(shape1=alpha,shape2=beta))
}
ab(a)
ab(b)
ab(c)
ggplot(data.frame(x=c(0,0.5,1),y=c(0,0.5,1)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = ab(a)[1], shape2 = ab(a)[2]),colour='#669900',size=1)
ggplot(data.frame(x=c(0,0.5,1),y=c(0,0.5,1)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = ab(b)[1], shape2 = ab(b)[2]),colour='#006666',size=1)
ggplot(data.frame(x=c(0,0.5,1),y=c(0,0.5,1)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = ab(c)[1], shape2 = ab(c)[2]),colour='#990099',size=1)

pat1<-pbeta(0.2,ab(a)[1],ab(a)[2])
pat2<-pbeta(0.4,ab(a)[1],ab(a)[2])-pbeta(0.2,ab(a)[1],ab(a)[2])
pat3<-pbeta(0.6,ab(a)[1],ab(a)[2])-pbeta(0.4,ab(a)[1],ab(a)[2])
pat4<-pbeta(0.8,ab(a)[1],ab(a)[2])-pbeta(0.6,ab(a)[1],ab(a)[2])
pat5<-pbeta(1,ab(a)[1],ab(a)[2])-pbeta(0.8,ab(a)[1],ab(a)[2])

pat<-c(pat1,pat2,pat3,pat4,pat5)

pbt1<-pbeta(0.2,ab(b)[1],ab(b)[2])
pbt2<-pbeta(0.4,ab(b)[1],ab(b)[2])-pbeta(0.2,ab(b)[1],ab(b)[2])
pbt3<-pbeta(0.6,ab(b)[1],ab(b)[2])-pbeta(0.4,ab(b)[1],ab(b)[2])
pbt4<-pbeta(0.8,ab(b)[1],ab(b)[2])-pbeta(0.6,ab(b)[1],ab(b)[2])
pbt5<-pbeta(1,ab(b)[1],ab(b)[2])-pbeta(0.8,ab(b)[1],ab(b)[2])

pbt<-c(pbt1,pbt2,pbt3,pbt4,pbt5)

pct1<-pbeta(0.2,ab(c)[1],ab(c)[2])
pct2<-pbeta(0.4,ab(c)[1],ab(c)[2])-pbeta(0.2,ab(c)[1],ab(c)[2])
pct3<-pbeta(0.6,ab(c)[1],ab(c)[2])-pbeta(0.4,ab(c)[1],ab(c)[2])
pct4<-pbeta(0.8,ab(c)[1],ab(c)[2])-pbeta(0.6,ab(c)[1],ab(c)[2])
pct5<-pbeta(1,ab(c)[1],ab(c)[2])-pbeta(0.8,ab(c)[1],ab(c)[2])

pct<-c(pct1,pct2,pct3,pct4,pct5)

chisq.test(at/2,pat)
chisq.test(bt/2,pbt)
chisq.test(ct/2,pct)

ggplot(data.frame(x=c( 0,0.5,1),y=c(0,0.5,1)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = 0.5, shape2 = 2.7),colour='#FF9933',size=1)+scale_x_continuous(limits = c(0,5))+scale_y_continuous(limits = c(0,5))
ggplot(data.frame(x=c(0,0.5,1),y=c(0,0.5,1)),aes(x=x,y=y))+scale_x_continuous(limits = c(0,5))+scale_y_continuous(limits = c(0,5))+stat_function(fun = dbeta, args = list(shape1 = 0.43, shape2 = 2.67),colour='#FF33FF',size=1)+stat_function(fun = dbeta, args = list(shape1 = 0.5, shape2 = 2.7),colour='#FF9933',size=1)

dane5<-list(data.table(dane3[[1]]),data.table(dane3[[2]]),data.table(dane3[[3]]))
dane5[[1]]<-subset(dane5[[1]],x!=y&y==49)
dane5[[2]]<-subset(dane5[[2]],x!=y&y==37)
dane5[[3]]<-subset(dane5[[3]],x!=y&y==47)
JW<-dane5[[1]]
EW<-dane5[[2]]
ACh<-dane5[[3]]
JW<-as.numeric(as.matrix(JW)[,3])
ACh<-as.numeric(as.matrix(ACh)[,3])
obl<-dane3[[1]]


ggplot(data.frame(x=c(0,0.5,1),y=c(0,0.5,1)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = ab(a)[1], shape2 = ab(a)[2]),colour='#669900',size=1)+theme_classic()
ggplot(data.frame(x=c(0,0.5,1),y=c(0,0.5,1)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = ab(b)[1], shape2 = ab(b)[2]),colour='#006666',size=1)+theme_classic()
ggplot(data.frame(x=c(0,0.5,1),y=c(0,0.5,1)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = ab(c)[1], shape2 = ab(c)[2]),colour='#990099',size=1)+theme_classic()

ggplot(dane4[[1]],aes(x=similarity))+stat_bin(fill='#669900',breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_classic()
ggplot(dane4[[2]],aes(x=similarity))+stat_bin(fill='#006666',breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_classic()
ggplot(dane4[[3]],aes(x=similarity))+stat_bin(fill='#990099',breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_classic()

names(dane4[[1]])<-c("x","y","podobie?stwo")
ggplot(dane4[[1]],aes(x=podobie?stwo))+stat_bin(fill='#669900',breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_classic()+ylab('liczno??')
names(dane4[[2]])<-c("x","y","podobie?stwo")
ggplot(dane4[[2]],aes(x=podobie?stwo))+stat_bin(fill='#006666',breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_classic()+ylab('liczno??')
names(dane4[[3]])<-c("x","y","podobie?stwo")
ggplot(dane4[[3]],aes(x=podobie?stwo))+stat_bin(fill='#990099',breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_classic()+ylab('liczno??')


names(dane3[[1]])<-c("x","y","podobie?stwo")
names(dane3[[2]])<-c("x","y","podobie?stwo")
names(dane3[[3]])<-c("x","y","podobie?stwo")

ggplot(dane3[[1]],aes(x=x,y=y))+geom_tile(aes(fill=podobie?stwo),colour="#FFCC33")+scale_fill_gradient2(low="#3399FF",mid="#FFCC33",high="#CC0000",guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(0,1),midpoint=0.5)+theme_minimal()+labs(x=NULL,y=NULL,fill='podobie?stwo')+scale_x_discrete(limits=c(1:49))
ggplot(dane3[[2]],aes(x=x,y=y))+geom_tile(aes(fill=podobie?stwo),colour="#FFCC33")+scale_fill_gradient2(low="#3399FF",mid="#FFCC33",high="#CC0000",guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(0,1),midpoint=0.5)+theme_minimal()+labs(x=NULL,y=NULL,fill='podobie?stwo')+scale_x_discrete(limits=c(1:37))
ggplot(dane3[[3]],aes(x=x,y=y))+geom_tile(aes(fill=podobie?stwo),colour="#FFCC33")+scale_fill_gradient2(low="#3399FF",mid="#FFCC33",high="#CC0000",guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(0,1),midpoint=0.5)+theme_minimal()+labs(x=NULL,y=NULL,fill='podobie?stwo')+scale_x_discrete(limits=c(1:47))


ab(a)
JW2<-subset(data.table(dane3[[1]]),x!=y&x!=49&y!=49)
mean(as.matrix(subset(JW2,y==1)[,3]))
obliczenia<-c(mean(as.matrix(subset(JW2,y==1)[,3])))
for(i in 2:48)
{
  obliczenia[i]<-mean(as.matrix(subset(JW2,y==i)[,3]))
}

ggplot(data.frame(x=obliczenia),aes(x))+stat_bin(fill='#CC6600',breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_classic()
ggplot(data.frame(x=c(0,0.5,1),y=c(0,0.5,1)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = ab(obliczenia)[1], shape2 = ab(obliczenia)[2]),colour='#CC6600',size=1)+theme_classic()


#test
p2<-table(cut(b,breaks=c(-0.01,0.2,0.4,0.6,0.8,1)))
pt1<-pbeta(0.2,ab(obliczenia)[1],ab(obliczenia)[2])
pt2<-pbeta(0.4,ab(obliczenia)[1],ab(obliczenia)[2])-pbeta(0.2,ab(obliczenia)[1],ab(obliczenia)[2])
pt3<-pbeta(0.6,ab(obliczenia)[1],ab(obliczenia)[2])-pbeta(0.4,ab(obliczenia)[1],ab(obliczenia)[2])
pt4<-pbeta(0.8,ab(obliczenia)[1],ab(obliczenia)[2])-pbeta(0.6,ab(obliczenia)[1],ab(obliczenia)[2])
pt5<-pbeta(1,ab(obliczenia)[1],ab(obliczenia)[2])-pbeta(0.8,ab(obliczenia)[1],ab(obliczenia)[2])
pt<-c(pt1,pt2,pt3,pt4,pt5)
#test
chisq.test(p2,pt)