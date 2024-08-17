sheetnames<-excel_sheets(path)
dane <- lapply(excel_sheets(path), read_excel, path = path)
names(dane) <- sheetnames
dane<-lapply(dane,data.frame)
dane2<-dane
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
        'z'=26)}
macierz <- function(x)
{
  x[,1]<-NULL
  x<-as.matrix(x)
  x<-sapply(x,use.switch)
  x<-matrix(x,29,5)
  x<-cbind(x,matrix(0,29,10))
}
dane2<-lapply(dane2,macierz)
for(i in 1:22)
{
  faces(dane2[[i]],main=i)
}
