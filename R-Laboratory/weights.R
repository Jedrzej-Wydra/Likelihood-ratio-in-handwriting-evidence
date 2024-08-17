library("stringr", lib.loc="D:/R/R-3.5.3/library")
library("tidyr", lib.loc="D:/R/R-3.5.3/library")
library(readxl)
library(data.table)
library("OmicsMarkeR", lib.loc="D:/R/R-3.5.3/library")
library(ggplot2)
sheetnames<-excel_sheets(path)
dane <- lapply(excel_sheets(path), read_excel, path = path)
names(dane) <- sheetnames
dane<-lapply(dane,data.frame)
dane2<-dane
ori<-c('ori','a','a','a','a','a')
dod<-function(x)
{
  x<-rbind(x,ori)
}
dane<-lapply(dane,dod)
rm<-dane[[1]]$falsifier
use.switch <- function(x)
{switch(x,
        'a'=1,
        'b'=2,
        'c'=3,
        'd'=4,
        'e'=5,
        'f'=6,
        'g'=7,
        'h'=8,
        'i'=9,
        'j'=10,
        'k'=11,
        'l'=12,
        'm'=13,
        'n'=14,
        'o'=15,
        'p'=16,
        'q'=17,
        'r'=18,
        's'=19,
        't'=20,
        'u'=21,
        'v'=22,
        'w'=23,
        'x'=24,
        'y'=25,
        'z'=26,
        '-'=27,
        x)}
macierz <- function(x)
{
  rm<-x[,1]
  x[,1]<-NULL
  x<-as.matrix(x)
  x<-sapply(x,use.switch)
  x<-matrix(x,30,5)
}
dane<-lapply(dane,macierz)

weights<-function(x,y)
{
  c1<-matrix(rep(x[,1],y[1]),30)
  c2<-matrix(rep(x[,2],y[2]),30)
  c3<-matrix(rep(x[,3],y[3]),30)
  c4<-matrix(rep(x[,4],y[4]),30)
  c5<-matrix(rep(x[,5],y[5]),30)
  x<-cbind(c1,c2,c3,c4,c5)
  return(x)
}
library(readxl)
wagi <- read_excel(path, 
                   col_names = FALSE)
wagi<-as.matrix(wagi)
for(i in 1:22)
{
  dane[[i]]<-weights(dane[[i]],wagi[i,])
}

for(i in 1:22)
{
  for(j in 1:dim(dane[[i]])[2])
  {dane[[i]][,j]<-dane[[i]][,j]+(j/100)}
}
for(i in 1:22)
{
  dane[[i]]<-matrix(sapply(dane[[i]],factor),30)
}

smx<-function(x)
{
  a=x
  c=dim(x)[2]
  for(i in 2:30)
  {a=rbind(a,x)}
  b=t(matrix(rep(x[1,],30),c,30))
  for(i in 2:30)
  {
    b=rbind(b,t(matrix(rep(x[i,],30),c,30)))
  }
  mx=cbind(a,b)
  return(mx)
}
pod<-function(x)
{
  x=smx(x)
  d=dim(x)[2]
  c=d/2
  a=jaccard(x[1,1:c],x[1,(c+1):d])
  for(i in 2:900)
  {
    a=c(a,jaccard(x[i,1:c],x[i,(c+1):d]))
  }
  x=matrix(a,30,30)
}
dane<-lapply(dane,pod)
dane<-lapply(dane,as.data.frame)
kolu<-function(x)
{
  x<-data.frame(nr=1:30, x)
}
dane<-lapply(dane, kolu)
dlugi<-function(x)
{
  
  x %>% gather('nr2','similarity','V1','V2','V3','V4','V5','V6','V7','V8','V9','V10','V11','V12','V13','V14','V15','V16','V17','V18','V19','V20','V21','V22','V23','V24','V25','V26','V27','V28','V29','V30') -> x
  return(x)
}
dane3<-lapply(dane,dlugi)
for(i in 1:22)
{
  dane3[[i]]$numer<-rep(i,30)
}
wyk<-dane3[[1]]
for(i in 2:22)
{
  wyk<-rbind(wyk,dane3[[i]])
}
use.switch2 <- function(x)
{switch(x,
        'V1'=1,
        'V2'=2,
        'V3'=3,
        'V4'=4,
        'V5'=5,
        'V6'=6,
        'V7'=7,
        'V8'=8,
        'V9'=9,
        'V10'=10,
        'V11'=11,
        'V12'=12,
        'V13'=13,
        'V14'=14,
        'V15'=15,
        'V16'=16,
        'V17'=17,
        'V18'=18,
        'V19'=19,
        'V20'=20,
        'V21'=21,
        'V22'=22,
        'V23'=23,
        'V24'=24,
        'V25'=25,
        'V26'=26,
        'V27'=27,
        'V28'=28,
        'V29'=29,
        'V30'=30,
        x)}
wyk$nr2=sapply(wyk$nr2,use.switch2)
ggplot(wyk,aes(nr,nr2))+geom_tile(aes(fill=similarity),colour="#3399FF")+scale_fill_gradient2(low="#3399FF",mid="#FFCC33",high="#CC0000",guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(0,1),midpoint=0.5)+theme_minimal()+labs(x=NULL,y=NULL,fill='similarity')+facet_wrap(~numer,ncol=4)+theme(text = element_text(size=20))
zak<-function(x)
{
    x<-round(x,digits = 3)
}
dane4<-lapply(dane,zak)
usu<-function(x)
{
  x<-x[30,]
}
dane4<-lapply(dane4,usu)
usu2<-function(x)
{
  x<-as.matrix(x[2:30])
}
dane4<-lapply(dane4,usu2)
for(i in 1:22)
{
  dane4[[i]]<-data.frame(value=1:29,similarity=t(dane4[[i]]),nr=rep(i,length(dane4[[i]])))
}
for(i in 1:22)
{
  names(dane4[[i]])<-c('value','similarity','nr')
}
wyk2<-dane4[[1]]
for(i in 2:22)
{
  wyk2<-rbind(wyk2,dane4[[i]])
}
wyk2$nr<-factor(wyk2$nr)
ggplot(wyk2,aes(x=similarity))+geom_bar(aes(fill=nr))+facet_wrap(~nr)+theme(legend.position = 'none')+labs(x='similarity',y='count')
ggplot(wyk2,aes(x=similarity))+stat_bin(breaks=c(0,0.2,0.4,0.6,0.8,1),aes(fill=nr))+facet_wrap(~nr)+theme(legend.position = 'none')+labs(x='similarity',y='count')+scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))
ggplot(wyk2,aes(x=similarity))+stat_bin(breaks=c(0,0.2,0.4,0.6,0.8,1),fill='#9966FF')+facet_wrap(~nr,ncol=3)+theme(legend.position = 'none')+scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_minimal()+theme(text = element_text(size=20))+xlab('Similarity between original and simulated signatures')+ylab('Number of simulated signatures')
ggplot(wyk2,aes(x=similarity))+stat_bin(breaks=c(0,0.2,0.4,0.6,0.8,1),fill='#9966FF')+theme(legend.position = 'none')+labs(x='similarity',y='count')+scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_classic()+xlab('Similarity between original and simulated signatures')+ylab('Number of simulated signatures')


#Kaliszak

Kali <- read_excel(path)
Kali <- Kali[,-1]
names(Kali)<-NULL
Kali<-as.data.frame(Kali)
weights2<-function(x,y)
{
  k2<-NULL
  for(i in 1:21)
  {
  k1=matrix(rep(x[,i],y[i]),29)
  k2=cbind(k2,k1)
  }
  return(k2)
}

wagik <- read_excel(path, col_names = FALSE)
wagik<-as.matrix(wagik)
wagik<-c(wagik)
Kali<-weights2(Kali,wagik)
for(i in 1:dim(Kali)[2])
{
  
  Kali[,i]<-str_c(factor(rep(i, 29)),Kali[,i])
}
d<-NULL
a<-Kali
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
Kali2<-cbind(b,f)

a<-Kali2
b<-sqrt(dim(a)[1])
d=dim(a)[2]
c=d/2
f<-jaccard(a[1,1:c],a[1,(c+1):d])
#f<-jaccard(a[1,1:40],a[1,41:80])
for(j in 2:dim(a)[1])
{
  f<-c(f,jaccard(a[j,1:c],a[j,(c+1):d]))
#  f<-c(f,jaccard(a[j,1:40],a[j,41:80]))
}
Kali2<-data.frame(matrix(f,b))
Kali2<-data.frame(nr=1:29,Kali2)
names(Kali2)<-c('nr',1:29)
Kali2 %>% gather(nr,similarity,as.character(1:29)) -> Kali3
Kali3<-data.frame(nr1=rep(1:29,29),Kali3)
Kali3$nr<-as.numeric(Kali3$nr)
ggplot(Kali3,aes(nr1,nr))+geom_tile(aes(fill=similarity),colour="#000000")+scale_fill_gradient2(low="#3399FF",mid="#FFCC33",high="#CC0000",guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(0,1),midpoint=0.5)+theme_minimal()+labs(x=NULL,y=NULL,fill='similarity')+scale_x_discrete(limits=c(1:29))+scale_y_discrete(limits=c(1:29))
ab<-function(x)
{
  m<-mean(x)
  v<-var(x)
  alpha<-(((m^2)-(m^3))/v)-m
  beta<-((m-2*(m^2)+(m^3))/v)+m-1
  return(c(shape1=alpha,shape2=beta))
}
Shapes<-as.data.frame(subset(Kali3, nr1!=29&nr!=29&nr1!=nr)$similarity)
Shapes2<-as.data.frame(subset(Kali3, nr1==29&nr1!=nr)$similarity)
names(Shapes2)<-c('similarity')
names(Shapes)<-c('similarity')
shapes<-ab(subset(Kali3, nr1!=29&nr!=29&nr1!=nr)$similarity)
ggplot(data.frame(x=c(0,0.5,1),y=c(0,0.5,1)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = shapes[1], shape2 = shapes[2]),colour='#33CC00',size=1)+scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_minimal()
ggplot(Shapes,aes(x=similarity))+stat_bin(breaks=c(0,0.2,0.4,0.6,0.8,1),fill='#33CC00')+theme(legend.position = 'none')+labs(x='similarity',y='count')+scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_classic()
ggplot(Shapes2,aes(x=similarity))+stat_bin(breaks=c(0,0.2,0.4,0.6,0.8,1),fill='#33CC00')+theme(legend.position = 'none')+labs(x='similarity',y='count')+scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_classic()

Shapes_E<-ab(wyk2$similarity)
ggplot(data.frame(x=c(0,0.5,1),y=c(0,0.5,1)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = Shapes_E[1], shape2 = Shapes_E[2]),colour='#000033',size=1)

table(cut(wyk2$similarity,5))
o2<-table(cut(wyk2$similarity,5))
o<-table(cut(wyk2$similarity,5))
o<-o*5/501
x<-c(0,0,0.2,0.2,0.4,0.4,0.6,0.6,0.8,0.8,1,1)
y<-c(0,o[1],o[1],o[2],o[2],o[3],o[3],o[4],o[4],o[5],o[5],0)

h=data.frame(x,y)

ggplot(h,aes(x,y))+geom_polygon(fill='#9966FF')+stat_function(fun = dbeta, args = list(shape1 = Shapes_E[1], shape2 = Shapes_E[2]),colour='#000033',size=1.06)+scale_y_continuous(limits=c(0,6),breaks = c(0,1,2,3,4,5))+scale_x_continuous(limits=c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_minimal()
#test badan
pat1<-pbeta(0.2,Shapes_E[1],Shapes_E[2])
pat2<-pbeta(0.4,Shapes_E[1],Shapes_E[2])-pbeta(0.2,Shapes_E[1],Shapes_E[2])
pat3<-pbeta(0.6,Shapes_E[1],Shapes_E[2])-pbeta(0.4,Shapes_E[1],Shapes_E[2])
pat4<-pbeta(0.8,Shapes_E[1],Shapes_E[2])-pbeta(0.6,Shapes_E[1],Shapes_E[2])
pat5<-pbeta(1,Shapes_E[1],Shapes_E[2])-pbeta(0.8,Shapes_E[1],Shapes_E[2])

pat<-c(pat1,pat2,pat3,pat4,pat5)
#test Kaliszaka
p2<-table(cut(Shapes$similarity,breaks=c(-0.01,0.2,0.4,0.6,0.8,1)))
pt1<-pbeta(0.2,shapes[1],shapes[2])
pt2<-pbeta(0.4,shapes[1],shapes[2])-pbeta(0.2,shapes[1],shapes[2])
pt3<-pbeta(0.6,shapes[1],shapes[2])-pbeta(0.4,shapes[1],shapes[2])
pt4<-pbeta(0.8,shapes[1],shapes[2])-pbeta(0.6,shapes[1],shapes[2])
pt5<-pbeta(1,shapes[1],shapes[2])-pbeta(0.8,shapes[1],shapes[2])
pt<-c(pt1,pt2,pt3,pt4,pt5)
#test Kaliszaka
chisq.test(p2,pt)
#test badan
chisq.test(o2,pat)
LR=pbeta(0.2,shapes[1],shapes[2])/pbeta(0.2,Shapes_E[1],Shapes_E[2])
LR
library("dplyr", lib.loc="D:/R/R-3.5.3/library")
example<-as.data.frame(subset(Kali3, nr1!=29&nr!=29&nr1!=nr))
example %>% group_by(nr) %>% summarise(mean(similarity)) -> example
example$`mean(similarity)` %>% cut(breaks=c(0,0.2,0.4,0.6,0.8,1)) %>% table()
(pbeta(0.4,shapes[1],shapes[2])-pbeta(0.2,shapes[1],shapes[2]))/(pbeta(0.4,Shapes_E[1],Shapes_E[2])-pbeta(0.2,Shapes_E[1],Shapes_E[2]))
(pbeta(0.6,shapes[1],shapes[2])-pbeta(0.4,shapes[1],shapes[2]))/(pbeta(0.6,Shapes_E[1],Shapes_E[2])-pbeta(0.4,Shapes_E[1],Shapes_E[2]))

ggplot(h,aes(x,y))+stat_function(fun = dbeta, args = list(shape1 = 0.41, shape2 = 2.84),colour='#000033',size=1.06)+scale_y_continuous(limits=c(0,6),breaks = c(0,1,2,3,4,5))+scale_x_continuous(limits=c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_minimal()

ggplot(wyk2,aes(similarity))+stat_bin(aes(y=..density..),bins = 10)+geom_density()+stat_function(fun = dbeta, args = list(shape1 = 0.41, shape2 = 2.84),colour='#000033',size=1.06)+scale_y_continuous(limits = c(0,5))

ggplot(wyk2,aes(x=similarity))+stat_bin(bins=10,fill='#9966FF')+facet_wrap(~nr,ncol=3)+theme(legend.position = 'none')+scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_minimal()+theme(text = element_text(size=20))+xlab('Similarity between original and simulated signatures')+ylab('Number of simulated signatures')

ggplot(Shapes,aes(x=similarity))+stat_bin(aes(y=..density..),bins=15,fill='#336600')+geom_density(alpha=.25,fill='#FFFF00')+theme(legend.position = 'none')+labs(x='similarity',y='density')+scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_minimal()+scale_y_continuous(limits=c(0,4))

ggplot(Shapes,aes(x=similarity))+geom_density(alpha=.25,fill='#FFFF00')+theme(legend.position = 'none')+labs(x='similarity',y='density')+scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_minimal()+stat_function(fun = dbeta, args = list(shape1 = 8.22, shape2 = 10.40),colour='#336600',size=1.06)+scale_y_continuous(limits=c(0,4))

ggplot(Shapes,aes(x=similarity))+theme(legend.position = 'none')+labs(x='similarity',y='count')+scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_minimal()+stat_function(fun = dbeta, args = list(shape1 = 8.22, shape2 = 10.40),colour='#336600',size=1.06)+stat_function(fun = dbeta, args = list(shape1 = 0.41, shape2 = 2.84),colour='#000000',size=1.06)

ggplot(wyk2,aes(similarity))+stat_bin(aes(y=..density..),bins = 10,fill='#9966FF')+geom_density(alpha=.25,fill='#FFCCFF')+scale_y_continuous(limits = c(0,5))+theme_minimal()
ggplot(wyk2,aes(similarity))+geom_density(alpha=.25,fill='#FFCCFF')+stat_function(fun = dbeta, args = list(shape1 = 0.41, shape2 = 2.84),colour='#000033',size=1.06)+scale_y_continuous(limits = c(0,5))+theme_minimal()

shapes
Shapes_E

ggplot(wyk2,aes(x=similarity))+stat_bin(bins=6,fill='#9966FF')+facet_wrap(~nr,ncol=3)+theme(legend.position = 'none')+scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))+theme_minimal()+theme(text = element_text(size=20))+xlab('Similarity between original and simulated signatures')+ylab('Number of simulated signatures')
