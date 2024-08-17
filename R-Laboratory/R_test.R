#names(test)<-c(1:dim(test)[2])
#test<-as.matrix(test)
a<-test
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
test<-cbind(b,f)

a<-test
b<-sqrt(dim(a)[1])
f<-jaccard(a[1,1:17],a[1,11:20])
for(j in 2:dim(a)[1])
{
  f<-c(f,jaccard(a[j,1:17],a[j,18:34]))
}
test<-data.frame(matrix(f,b))
names(test)<-c(1:b)
rownames(test)<-c(1:b)
dane3<-melt(test, varnames=c('x','y'),value.name="similarity")
x<-rep(1:b,b)
dane3<-data.frame(x,dane3)
names(dane3)<-c('x','y','similarity')
ggplot(dane3,aes(x=x,y=y))+geom_tile(aes(fill=similarity),colour="#000000")+scale_fill_gradient2(low="#3399FF",mid="#FFCC33",high="#CC0000",guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(0,1),midpoint=0.5)+theme_minimal()+labs(x=NULL,y=NULL,fill='similarity')
ggplot(dane3,aes(x=similarity))+stat_bin(breaks = c(0,0.2,0.4,0.6,0.8,1))
