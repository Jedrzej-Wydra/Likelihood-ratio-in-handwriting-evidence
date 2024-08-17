library(readxl)
library(data.table)

sheetnames<-excel_sheets(path)
dane <- lapply(excel_sheets(path), read_excel, path = path)
names(dane) <- sheetnames
dane<-lapply(dane,data.table)
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
fact<-function(x)
{
  x$trudnosc<-factor(x$trudnosc)
  return(x)
}
dane<-lapply(dane,fact)
ggplot(dane$Obiekt_01,aes(x=numer,y=suma))+geom_col(aes(fill=trudno??))+scale_x_discrete(limit = c(1:22))+scale_y_continuous(breaks=c(1:7),limit = c(0, 7))+labs(x = "", fill = "Trudno????")
nazwy<-function(x,i)
{
  x$obiekt<-rep(i,22)
  return(x)
}
dane2<-dane
for(i in 1:29)
{
  dane2[[i]]<-nazwy(dane2[[i]],i)
}
wykres<-dane2[[1]]
for(i in 2:29)
{
  wykres<-rbind(wykres,dane2[[i]])
}
ggplot(wykres,aes(x=numer,y=suma))+geom_col(fill='#9966FF')+facet_wrap(~obiekt,ncol=3)+theme(legend.position = 'none')+theme_classic()+xlab('number of signature')+ylab('sum of variations')+theme(text = element_text(size=20))

ggplot(wykres,aes(x=obiekt,y=suma))+geom_col(fill='#999999')+facet_wrap(~numer,ncol=3)+theme(legend.position = 'none')+theme_classic()+xlab('number of subject')+ylab('sum of variations')

ggplot(wykres,aes(x=numer,y=suma))+geom_col(fill='#9966FF')+facet_wrap(~obiekt,ncol=3)+theme(legend.position = 'none')+theme_classic()+xlab('Number of a signature')+ylab('Number of differences')+theme(text = element_text(size=20))
