#library(readxl)#
#library(data.table)#
#library(ggplot2)#
sheetnames<-excel_sheets(path)
dane <- lapply(excel_sheets(path), read_excel, path = path)
names(dane) <- sheetnames
dane<-lapply(dane,data.frame)

dane2<-dane
dane3<-dane
dane5<-dane
for(i in 1:22)
{
  names(dane2[[i]])<-c('falsifier','C1','C2','C3','C4','C5')
  names(dane3[[i]])<-c('falsifier','C1','C2','C3','C4','C5')
  names(dane5[[i]])<-c('falsifier','C1','C2','C3','C4','C5')
  names(dane[[i]])<-c('falsifier','C1','C2','C3','C4','C5')
}
Chi<-function(x)
{
  Zm1<-chisq.test(table(x$C1)[-1])$p.value
  Zm2<-chisq.test(table(x$C2)[-1])$p.value
  Zm3<-chisq.test(table(x$C3)[-1])$p.value
  Zm4<-chisq.test(table(x$C4)[-1])$p.value
  Zm5<-chisq.test(table(x$C5)[-1])$p.value
  x<-cbind(Zm1,Zm2,Zm3,Zm4,Zm5)
  return(x)
}
dane2<-lapply(dane2,Chi)
dane2[[13]][,3]<-chisq.test(table(dane[[13]]$C3))$p.value
dane2[[15]][,1]<-chisq.test(table(dane[[15]]$C1))$p.value
dane2[[16]][,3]<-chisq.test(table(dane[[16]]$C3))$p.value
dane2[[20]][,2]<-chisq.test(table(dane[[20]]$C2))$p.value
testchi<-dane2[[1]]
for(i in 2:22)
{
  testchi<-rbind(testchi,dane2[[i]])
}
y<-rep(1:22,5)
x<-c(rep('C1',22),rep('C2',22),rep('C3',22),rep('C4',22),rep('C5',22))
pvar<-c(testchi[,1],testchi[,2],testchi[,3],testchi[,4],testchi[,5])
pvar<-ifelse(pvar<=0.05,NA,pvar)
wyk<-data.frame(x,y,pvar)
ggplot(wyk,aes(x=x,y=y))+geom_tile(aes(fill=pvar),colour="#000000")+scale_fill_gradient2(low="#990000",mid="#669900",high="#FFCC33",guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(0,1),midpoint=0.5)+theme_minimal()+labs(x=NULL,y=NULL)

Chit<-function(x)
{
  Zm1<-table(x$C1)[1]
  Zm2<-table(x$C2)[1]
  Zm3<-table(x$C3)[1]
  Zm4<-table(x$C4)[1]
  Zm5<-table(x$C5)[1]
  x<-c(Zm1,Zm2,Zm3,Zm4,Zm5)
  x<-chisq.test(x)$p.value
  return(x)
}
dane3<-lapply(dane3,Chit)
test<-dane3[[1]]
for(i in 2:22)
{
  test<-c(test,dane3[[i]])
}
sheetnames<-excel_sheets(path)
dane4 <- lapply(excel_sheets(path), read_excel, path = path)
names(dane4) <- sheetnames
dane4<-lapply(dane4,data.frame)
trudno??<-factor(dane4[[1]]$trudno??)
ggplot(data.frame(pvar=test,nr=c(1:22),trudno??=trudno??),aes(x=nr,y=pvar))+geom_col(aes(fill=nr))+geom_line(y=0.05,size=3,colour='#660000')+labs(x='podpis',y='p-warto??')+theme(legend.position = 'none')
test<-ifelse(test<=0.05,0.025,test)
ggplot(data.frame(pvar=test,nr=c(1:22),trudno??=trudno??),aes(x=nr,y=pvar))+geom_col(aes(fill=trudno??))+geom_line(y=0.05,size=3,colour='#660000')+labs(x='podpis',y='p-warto??',fill = "Trudno??")
prop<-function(x)
{
  Zm1<-table(x$C1)
  Zm2<-table(x$C2)
  Zm3<-table(x$C3)
  Zm4<-table(x$C4)
  Zm5<-table(x$C5)
  C1<-mean(Zm1[1]/Zm1[-1])
  C2<-mean(Zm1[2]/Zm1[-1])
  C3<-mean(Zm1[3]/Zm1[-1])
  C4<-mean(Zm1[4]/Zm1[-1])
  C5<-mean(Zm1[5]/Zm1[-1])
  x<-c(C1,C2,C3,C4,C5)
}
dane6<-lapply(dane5,prop)
#w poni?szych 'a' nie wyst?puje#
#'a' nie wyst?puje w#
#S13 C3#
#S15 C1#
#S16 C3#
#S20 C2#
dane6[[13]][3]<-0
dane6[[15]][1]<-0
dane6[[16]][3]<-0
dane6[[20]][2]<-0
dane7<-lapply(dane6,mean)
shap<-dane7[[1]]
for(i in 2:22)
{
  shap<-c(shap,dane7[[i]])
}
shapiro.test(shap)
round(mean(shap),digit=2)
prawdo<-c(2.1,rep(1,25))/sum(c(2.1,rep(1,25)))
Chi2<-function(x)
{
  Zm1<-chisq.test(c(table(x$C1),rep(0,26-length(table(x$C1)))),p=prawdo)$p.value
  Zm2<-chisq.test(c(table(x$C2),rep(0,26-length(table(x$C2)))),p=prawdo)$p.value
  Zm3<-chisq.test(c(table(x$C3),rep(0,26-length(table(x$C3)))),p=prawdo)$p.value
  Zm4<-chisq.test(c(table(x$C4),rep(0,26-length(table(x$C4)))),p=prawdo)$p.value
  Zm5<-chisq.test(c(table(x$C5),rep(0,26-length(table(x$C5)))),p=prawdo)$p.value
  x<-cbind(Zm1,Zm2,Zm3,Zm4,Zm5)
  return(x)
}
dane8<-lapply(dane,Chi2)
results<-dane8[[1]]
for(i in 2:22)
{
  results<-rbind(results,dane8[[i]])
}
results<-ifelse(results<=0.05,NA,results)

#max rozbie?no?ci nie pasuje do modelu, we?my ?redni? liczb? rozbie?no?ci#
proportion<-function(x)
{
  C1<-length(table(x$C1))
  C2<-length(table(x$C2))
  C3<-length(table(x$C3))
  C4<-length(table(x$C4))
  C5<-length(table(x$C5))
  x<-c(C1,C2,C3,C4,C5)
  return(x)
}
dane9<-lapply(dane,proportion)
macierz_wariant?w<-dane9[[1]]
for(i in 2:22)
{
  macierz_wariant?w<-rbind(macierz_wariant?w,dane9[[i]])
}
rownames(macierz_wariant?w)<-NULL
macierz_wariant?w
results
#wykres gor?ca dla hipotezy max#
y<-rep(1:22,5)
x<-c(rep('C1',22),rep('C2',22),rep('C3',22),rep('C4',22),rep('C5',22))
pvar2<-c(results[,1],results[,2],results[,3],results[,4],results[,5])
wyk2<-data.frame(x,y,pvar2)
ggplot(wyk2,aes(x=x,y=y))+geom_tile(aes(fill=pvar2),colour="#000000")+scale_fill_gradient2(low="#990000",mid="#669900",high="#FFCC33",guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(0,1),midpoint=0.5)+theme_minimal()+labs(x=NULL,y=NULL,fill='pvar')
#koniec wykresu#
m_w<-data.frame(macierz_wariant?w)
names(m_w)<-c('C1','C2','C3','C4','C5')
box<-data.frame(y=c(m_w[,1],m_w[,2],m_w[,3],m_w[,4],m_w[,5]),x=c(rep('C1',22),rep('C2',22),rep('C3',22),rep('C4',22),rep('C5',22)))
ggplot(box,aes(x=x,y=y))+stat_boxplot(geom ='errorbar')+geom_boxplot()+labs(y='',x='')
ggplot(box,aes(x=x,y=y))+geom_violin()+labs(y='',x='')
summary(m_w)
mean(data.matrix(m_w))
shapiro.test(c(14.73,13.27,15.18,14.27,17.05))
dlugo<-function(x)
{
  a<-ifelse(length(x)>=15,15,length(x))
  return(a)
}
dlugo2<-function(x)
{
  a<-ifelse(length(x)>=15,length(x)-1,14)
  return(a)
}
Chi3<-function(x)
{
  Zm1<-chisq.test(c(table(x$C1),rep(0,15-dlugo(table(x$C1)))),p=c(2.65,rep(1,dlugo2(table(x$C1))))/(sum(c(2.65,rep(1,dlugo2(table(x$C1)))))))$p.value
  Zm2<-chisq.test(c(table(x$C2),rep(0,15-dlugo(table(x$C2)))),p=c(2.65,rep(1,dlugo2(table(x$C2))))/(sum(c(2.65,rep(1,dlugo2(table(x$C2)))))))$p.value
  Zm3<-chisq.test(c(table(x$C3),rep(0,15-dlugo(table(x$C3)))),p=c(2.65,rep(1,dlugo2(table(x$C3))))/(sum(c(2.65,rep(1,dlugo2(table(x$C3)))))))$p.value
  Zm4<-chisq.test(c(table(x$C4),rep(0,15-dlugo(table(x$C4)))),p=c(2.65,rep(1,dlugo2(table(x$C4))))/(sum(c(2.65,rep(1,dlugo2(table(x$C4)))))))$p.value
  Zm5<-chisq.test(c(table(x$C5),rep(0,15-dlugo(table(x$C5)))),p=c(2.65,rep(1,dlugo2(table(x$C5))))/(sum(c(2.65,rep(1,dlugo2(table(x$C5)))))))$p.value
  x<-cbind(Zm1,Zm2,Zm3,Zm4,Zm5)
  return(x)
}
dane10<-lapply(dane,Chi3)
dopas<-dane10[[1]]
for(i in 2:22)
{
  dopas<-rbind(dopas,dane10[[i]])
}
dopas
#wykres gor?ca dla hipotezy mean#
y<-rep(1:22,5)
x<-c(rep('C1',22),rep('C2',22),rep('C3',22),rep('C4',22),rep('C5',22))
pvar3<-c(dopas[,1],dopas[,2],dopas[,3],dopas[,4],dopas[,5])
pvar3<-ifelse(pvar3<=0.05,NA,pvar3)
wyk3<-data.frame(x,y,pvar3)
ggplot(wyk3,aes(x=x,y=y))+geom_tile(aes(fill=pvar3),colour="#000000")+scale_fill_gradient2(low="#990000",mid="#669900",high="#FFCC33",guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(0,1),midpoint=0.5)+theme_minimal()+labs(x=NULL,y=NULL,fill='pvar')
#koniec wykresu#
sum(ifelse(is.na(pvar3),1,0))
Chi4<-function(x)
{
  Zm1<-chisq.test(c(table(x$C1)),p=c(2.65,rep(1,length(table(x$C1))-1))/(length(table(x$C1))+1.65))$p.value
  Zm2<-chisq.test(c(table(x$C2)),p=c(2.65,rep(1,length(table(x$C2))-1))/(length(table(x$C2))+1.65))$p.value
  Zm3<-chisq.test(c(table(x$C3)),p=c(2.65,rep(1,length(table(x$C3))-1))/(length(table(x$C3))+1.65))$p.value
  Zm4<-chisq.test(c(table(x$C4)),p=c(2.65,rep(1,length(table(x$C4))-1))/(length(table(x$C4))+1.65))$p.value
  Zm5<-chisq.test(c(table(x$C5)),p=c(2.65,rep(1,length(table(x$C5))-1))/(length(table(x$C5))+1.65))$p.value
  x<-cbind(Zm1,Zm2,Zm3,Zm4,Zm5)
  return(x)
}
dane11<-lapply(dane,Chi4)
dopas2<-dane11[[1]]
for(i in 2:22)
{
  dopas2<-rbind(dopas2,dane11[[i]])
}
pvar4<-c(dopas2[,1],dopas2[,2],dopas2[,3],dopas2[,4],dopas2[,5])
pvar4<-ifelse(pvar4<=0.05,NA,pvar4)
wyk4<-data.frame(x,y,pvar4)
ggplot(wyk4,aes(x=x,y=y))+geom_tile(aes(fill=pvar4),colour="#000000")+scale_fill_gradient2(low="#990000",mid="#669900",high="#FFCC33",guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(0,1),midpoint=0.5)+theme_minimal()+labs(x=NULL,y=NULL,fill='pvar')
sum(ifelse(is.na(pvar3),1,0))
sum(ifelse(is.na(pvar4),1,0))