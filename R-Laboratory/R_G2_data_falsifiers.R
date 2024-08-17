library(readxl)
library(data.table)
sheetnames<-excel_sheets(path)
dane <- lapply(excel_sheets(path), read_excel, path = path)
names(dane) <- sheetnames
dane<-lapply(dane,data.frame)
dane2<-dane
use.ifelse<-function(x)
{
  y=x[,2:11]
  podpis=x[,1]
  y=ifelse(y == 'a',
         0,
         1)
  y=data.frame(y)
  x=cbind(podpis,y)
}
dane2<-lapply(dane2,use.ifelse)
kolsum<-function(x)
{
  suma<-colSums(t(x[,2:11]))
  x$suma<-suma
  return(x)
}
dane2<-lapply(dane2,kolsum)
library(ggplot2)
kol<-function(x)
{
  numer<-c(1:4)
  x$numer<-numer
  return(x)
}
dane2<-lapply(dane2,kol)
nazwy<-function(x,i)
{
  x$obiekt<-rep(i,4)
  return(x)
}
for(i in 1:44)
{
  dane2[[i]]<-nazwy(dane2[[i]],i)
}
wykres<-dane2[[1]]
for(i in 2:44)
{
  wykres<-rbind(wykres,dane2[[i]])
}
typ<-rep(c(1,1,2,2),44)
typ<-factor(typ)
wykres<-cbind(wykres,typ)
ggplot(wykres,aes(x=numer,y=suma))+geom_col(aes(fill=typ),width=.5)+labs(x = "", fill = "Typ")+facet_wrap(~obiekt)
