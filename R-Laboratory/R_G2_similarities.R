#library(readxl)
#library(data.table)
#library("OmicsMarkeR", lib.loc="D:/R/R-3.5.3/library")
#library(ggplot2)
sheetnames<-excel_sheets(path)
dane <- lapply(excel_sheets(path), read_excel, path = path)
names(dane) <- sheetnames
dane<-lapply(dane,data.frame)
dane2<-dane
macierz <- function(x)
{
  rm<-x[,1]
  x[,1]<-NULL
  x<-as.matrix(x)
  x<-sapply(x,use.switch)
  x<-matrix(x,5,10)
}
dane<-lapply(dane,macierz)
dane<-lapply(dane,data.frame)
for(i in 1:44)
{
  names(dane[[i]])<-c('C1','C2','C3','C4','C5','C6','C7','C8','C9','C10')
}
for(i in 1:44)
{
  dane[[i]][,1]<-dane[[i]][,1]+0.1
  dane[[i]][,2]<-dane[[i]][,2]+0.2
  dane[[i]][,3]<-dane[[i]][,3]+0.3
  dane[[i]][,4]<-dane[[i]][,4]+0.4
  dane[[i]][,5]<-dane[[i]][,5]+0.5
  dane[[i]][,6]<-dane[[i]][,6]+0.6
  dane[[i]][,7]<-dane[[i]][,7]+0.7
  dane[[i]][,8]<-dane[[i]][,8]+0.8
  dane[[i]][,9]<-dane[[i]][,9]+0.9
  dane[[i]][,10]<-dane[[i]][,10]
}
for(i in 1:44)
{
  dane[[i]]<-sapply(dane[[i]],factor)
}
smx<-function(x)
{
  a=x
  for(i in 2:5)
  {a=rbind(a,x)}
  b=t(matrix(rep(x[1,],5),10,5))
  for(i in 2:5)
  {
    b=rbind(b,t(matrix(rep(x[i,],5),10,5)))
  }
  mx=cbind(a,b)
  return(mx)
}
pod<-function(x)
{
  x=smx(x)
  a=jaccard(x[1,1:10],x[1,11:20])
  for(i in 2:25)
  {
    a=c(a,jaccard(x[i,1:10],x[i,11:20]))
  }
  x=matrix(a,5,5)
  return(x)
}
dane3<-dane
for(i in 1:44)
{
  dane3[[i]]<-pod(dane[[i]])
}
prze<-function(x)
{
  x<-melt(x,varnames=c('y','z'),value.name='similarity')
}
dane4<-lapply(dane3,prze)
a<-names(dane)
for(i in 1:44)
{
  dane4[[i]]$numer<-a[i]
}
wyk<-dane4[[1]]
for(i in 2:44)
{
  wyk<-rbind(wyk,dane4[[i]])
}
ggplot(wyk,aes(x=y,y=z))+geom_tile(aes(fill=similarity),colour="#000000")+scale_fill_gradient2(low="#3399FF",mid="#FFCC33",high="#CC0000",guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(0,1),midpoint=0.5)+scale_x_discrete(limits=c('P1','P2','S1','S2','W'))+scale_y_discrete(limits=c('P1','P2','S1','S2','W'))+theme_minimal()+labs(x=NULL,y=NULL,fill='similarity')+facet_wrap(~numer)
ggplot(wyk,aes(x=y,y=z))+geom_tile(aes(fill=similarity),colour="#3399FF")+scale_fill_gradient2(low="#3399FF",mid="#FFCC33",high="#CC0000",guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(0,1),midpoint=0.5)+scale_x_discrete(limits=c('P1','P2','S1','S2','W'))+scale_y_discrete(limits=c('P1','P2','S1','S2','W'))+theme_minimal()+labs(x=NULL,y=NULL,fill='podobie?stwo')+facet_wrap(~numer,ncol=5)+theme(text = element_text(size=20))
