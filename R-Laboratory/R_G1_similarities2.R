sheetnames<-excel_sheets(path)
dane1 <- lapply(excel_sheets(path), read_excel, path = path)
names(dane1) <- sheetnames
dane1<-lapply(dane1,data.frame)

sheetnames<-excel_sheets(path)
dane2 <- lapply(excel_sheets(path), read_excel, path = path)
names(dane2) <- sheetnames
dane2<-lapply(dane2,data.frame)

for(i in 1:29)
{names(dane2[[i]])<-NULL}
for(i in 1:29)
{names(dane1[[i]])<-NULL}

wyk<-NULL
for(Obiekt in 1:29)
{
  for(Podpis in 1:22)
  {
    wyk<-rbind(wyk, cbind(dane1[[Podpis]][Obiekt,],Podpis,Obiekt),cbind(dane2[[Obiekt]][Podpis,],Podpis,Obiekt),c('ori',rep('a',5),Podpis,Obiekt))
  }
}
rownames(wyk)<-NULL
wyk<-wyk[,-1]
names(wyk)<-c(1:5,'Podpis','Obiekt')
wyk2<-as.matrix(wyk[,1:5])
wyk2<-apply(wyk2,FUN=use.switch,MARGIN=c(1,2))
wyk2<-apply(wyk2,FUN=as.numeric,MARGIN=c(1,2))

wyk2[,1]<-wyk2[,1]+0.1
wyk2[,2]<-wyk2[,2]+0.2
wyk2[,3]<-wyk2[,3]+0.3
wyk2[,4]<-wyk2[,4]+0.4
wyk2[,5]<-wyk2[,5]+0.5

wyk2<-apply(wyk2,FUN=factor,MARGIN=c(1,2))
wyk[,1:5]<-wyk2
head(wyk)
wyk<-data.table(wyk)

w<-as.matrix(wyk)
w2<-list(NULL)
a<-list(NULL)
for(i in 1:29)
{
  for(j in 1:22)
  {
    a[[j]]<-wyk[Podpis==factor(j)&Obiekt==factor(i)]
  }
  w2[[i]]<-a
}
for(i in 1:29)
{
  for(j in 1:22)
  {
    a<-w2[[i]][[j]][1,]
    b<-w2[[i]][[j]][2,]
    c<-w2[[i]][[j]][3,]
    d<-rbind(a,a,a,b,b,b,c,c,c)
    w2[[i]][[j]]<-rbind(w2[[i]][[j]],w2[[i]][[j]],w2[[i]][[j]])
    w2[[i]][[j]]<-cbind(w2[[i]][[j]],d)
    w2[[i]][[j]]<-w2[[i]][[j]][,-14]
    w2[[i]][[j]]<-w2[[i]][[j]][,-13]
    e<-as.matrix(w2[[i]][[j]])
    f<-c('P','S','O','P','S','O','P','S','O')
    g<-c('P','P','P','S','S','S','O','O','O')
    h<-jaccard(e[1,1:5],e[1,8:12])
    for(l in 2:9)
    {
      h<-c(h,jaccard(e[l,1:5],e[l,8:12]))
    }
    w2[[i]][[j]]<-data.table(f,g,h,e[,6:7])
  }
}
z<-list(NULL)
for(i in 1:29)
{
  wykres<-w2[[i]][[1]]
  for(j in 1:22)
  {
    wykres<-rbind(wykres,w2[[i]][[j]])
  }
  z[[i]]<-wykres
}
wykres<-z[[1]]
for(i in 2:29)
{
  wykres<-rbind(wykres,z[[i]])
}
head(wykres)
wykres$Podpis<-as.numeric(wykres$Podpis)
wykres$Obiekt<-as.numeric(wykres$Obiekt)
ggplot(wykres,aes(x=f,y=g))+geom_tile(aes(fill=h),colour="#000000")+scale_fill_gradient2(low="#3399FF",mid="#FFCC33",high="#CC0000",guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(0,1),midpoint=0.5)+theme_minimal()+labs(x=NULL,y=NULL,fill='similarity')+facet_grid(Podpis~Obiekt)
