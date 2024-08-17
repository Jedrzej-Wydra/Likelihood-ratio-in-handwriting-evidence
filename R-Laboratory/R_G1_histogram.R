sheetnames<-excel_sheets(path)
dane <- lapply(excel_sheets(path), read_excel, path = path)
names(dane) <- sheetnames
dane<-lapply(dane,data.frame)
zmiana<-function(x)
{
  names(x)<-NULL
  Zm1<-rep(x[,1],5)
  Zm2<-c(x[,2],x[,3],x[,4],x[,5],x[,6])
  Zm3<-c(rep('C1',29),rep('C2',29),rep('C3',29),rep('C4',29),rep('C5',29))
  x<-cbind(Zm1,Zm2,Zm3)
  return(x)
}
dane<-lapply(dane,zmiana)
dane<-lapply(dane,data.frame)
for( i in 1:22)
{
  numer<-rep(i,145)
  dane[[i]]$numer<-numer
}
wykres<-dane[[1]]
for(i in 2:22)
{
  wykres<-rbind(wykres,dane[[i]])
}
ggplot(wykres,aes(x=Zm2))+geom_bar(aes(fill=Zm3))+xlab("warto??? cechy")+ylab("liczno??")+facet_grid(Zm3~numer)+theme(legend.position = "none")+scale_x_discrete(labels=NULL)
#ggplot(dane[[i]],aes(x=Zm2))+geom_bar(position="dodge",aes(fill=Zm3))+xlab("warto??? cechy")+ylab("liczno??")
#ggplot(dane[[i]],aes(x=Zm2))+geom_bar(position="dodge",aes(fill=Zm3))+xlab("warto??? cechy")+ylab("liczno??")+facet_wrap(~Zm3)
