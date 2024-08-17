sheetnames<-excel_sheets(path)
dane <- lapply(excel_sheets(path), read_excel, path = path)
names(dane) <- sheetnames
dane<-lapply(dane,data.frame)
dane[[2]]<-dane[[2]][-38,]
for(i in 1:3)
{
  dane[[i]]<-dane[[i]][,-1]
}

kor<-function(x)
{
  x<-as.matrix(x)
  x<-apply(x,FUN=use.switch,MARGIN = c(1,2))
  x<-apply(x,FUN=as.numeric,MARGIN = c(1,2))
  x[,1]<-x[,1]+0.1
  x[,2]<-x[,2]+0.2
  x[,3]<-x[,3]+0.3
  x[,4]<-x[,4]+0.4
  x[,5]<-x[,5]+0.5
  x[,6]<-x[,6]+0.6
  x[,7]<-x[,7]+0.7
  x[,8]<-x[,8]+0.8
  x[,9]<-x[,9]+0.9
  x[,10]<-x[,10]
  x<-apply(x,FUN=factor,MARGIN = c(1,2))
  x<-data.frame(x)
}
dane<-lapply(dane,kor)
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
  f<-jaccard(a[1,1:10],a[1,11:20])
  for(j in 2:dim(a)[1])
  {
    f<-c(f,jaccard(a[j,1:10],a[j,11:20]))
  }
  dane[[i]]<-data.frame(matrix(f,b))
  names(dane[[i]])<-c(1:b)
  rownames(dane[[i]])<-c(1:b)
  dane3[[i]]<-melt(dane[[i]], varnames=c('x','y'),value.name="similarity")
  x<-rep(1:b,b)
  dane3[[i]]<-data.frame(x,dane3[[i]])
  names(dane3[[i]])<-c('x','y','similarity')
}
ggplot(dane3[[1]],aes(x=x,y=y))+geom_tile(aes(fill=similarity),colour="#000000")+scale_fill_gradient2(low="#3399FF",mid="#FFCC33",high="#CC0000",guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(0,1),midpoint=0.5)+theme_minimal()+labs(x=NULL,y=NULL,fill='similarity')
ggplot(dane3[[1]],aes(x=similarity))+stat_bin(breaks = c(0,0.2,0.4,0.6,0.8,1))
