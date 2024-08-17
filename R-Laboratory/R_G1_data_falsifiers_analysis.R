sheetnames<-excel_sheets(path)
dane <- lapply(excel_sheets(path), read_excel, path = path)
names(dane) <- sheetnames
dane<-lapply(dane,data.table)
dane2<-dane
kolsum<-function(x)
{
  suma<-colSums(t(x[,2:6]))
  x$suma<-suma
  return(x)
}
dane<-lapply(dane,kolsum)
library(ggplot2)
kol<-function(x)
{
  numer<-c(1:22)
  x$numer<-numer
  return(x)
}
dane<-lapply(dane,kol)
for(i in 1:29)
{
  dane[[i]]<-dane[[i]][,8:9]
}
Chi<-function(x)
{
  Zm1<-chisq.test(x$suma)$p.value
  x<-Zm1
  return(x)
}
dane<-lapply(dane,Chi)
testchi<-dane[[1]]
for(i in 2:29)
{
  testchi<-c(testchi,dane[[i]])
}
wyk<-data.frame(c(1:29),testchi)
names(wyk)<-c('nr','pvar')
ggplot(wyk,aes(x=nr,y=pvar))+geom_col(aes(fill=nr))+geom_line(y=0.05,size=3,colour='#660000')+theme(legend.position = "none")+labs(x='obiekt',y='p-warto??')
