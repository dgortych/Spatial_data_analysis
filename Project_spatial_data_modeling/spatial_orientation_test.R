library("sp")
library("raster")
library("dismo")
library("dplyr")
library("ggvoronoi")
library("ggplot2")

#wczytanie danych
setwd("D:/Semestr_5/ModelowanieDanychPrzestrzennych/projekt")
dane_wiktor<-read.table("WiktorS_output.txt", header = TRUE, sep=";")
dane_damian<-read.table("DamianG2t_output_0.txt", header = TRUE, sep=";")

cartesian_product_wiktor<-read.csv("cartesian_product7.csv", header=FALSE,sep=" ")
names(cartesian_product_wiktor)<-c("y","x","z","id")
cartesian_product_damian<-read.csv("cartesian_product5.csv", header=FALSE,sep=" ")
names(cartesian_product_damian)<-c("y","x","z","id")

dane_wiktor<-data.frame(dane_wiktor)
dane_damian<-data.frame(dane_damian)

#utworzenie zbioru losywych danych
dane_xyz_wiktor<-dane_wiktor[0:3]
dane_xyz_damian<-dane_damian[0:3]
los_dane_xyz_wiktor<-sample_n(dane_xyz_wiktor,5000)
los_dane_xyz_damian<-sample_n(dane_xyz_damian,5000)
los_dane_xyz_wiktor <-unique(los_dane_xyz_wiktor)
los_dane_xyz_damian <-unique(los_dane_xyz_damian)


#voronoi
ggplot(los_dane_xyz_wiktor)+
  xlab(" ")+ylab(" ")+
  geom_voronoi(aes(Y1,X1, fill=Z1))+
  scale_fill_gradientn(" ",colors=c("darkblue","green","yellow","orange"),
                       values=scales::rescale(c(-50,50,100,200))) 
  
  
ggplot(los_dane_xyz_damian)+
  xlab(" ")+ylab(" ")+
  geom_voronoi(aes(Y1,X1, fill=Z1))+
  scale_fill_gradientn(" ",colors=c("blue4","deepskyblue"),
                       values=scales::rescale(c(-6000,-5200)))
  
  


#czyszczenie danych do wczytania do CloudCompare
dane_wyczyszczone_wiktor<-dane_wiktor[dane_wiktor$Dip_dir!='x',] 
write.table(dane_wyczyszczone_wiktor,"dane_wyczyszczone_wiktor.txt", quote=F, sep=";")
#u³amek danych usuniêtych
1-((dim(dane_wyczyszczone_wiktor))[1]/(dim(dane_wiktor))[1])


dane_wyczyszczone_damian<-dane_damian[dane_damian$Dip_dir!='x',] 
write.table(dane_wyczyszczone_damian,"dane_wyczyszczone_damian.txt", quote=F, sep=";")
1-((dim(dane_wyczyszczone_damian))[1]/(dim(dane_damian))[1])

#test orientacji
test_orientacji<-function(px,py,qx,qy,rx,ry)
{
  return(sign((qx*ry+px*qy+py*rx-px*ry-rx*qy-py*qx)))
}


nsize<-dim(cartesian_product_wiktor)[1]
lista=rep(0,nsize)
for (i in 1:nsize) {
  lista[i]<- test_orientacji(1750000,2100000,1900000,2200000,cartesian_product_wiktor$x[i],cartesian_product_wiktor$y[i])
}

ggplot(cartesian_product_wiktor)+
  xlab(" ")+ylab(" ")+
  geom_point(aes(x,y, color=factor(lista)))+
  scale_color_manual(" ",breaks = c("-1", "1"),
                     values=c("red", "blue"))


nsize<-dim(cartesian_product_damian)[1]
lista=rep(0,nsize)
for (i in 1:nsize) {
  lista[i]<- test_orientacji(-7300000,6200000,-7300000,5900000,cartesian_product_damian$x[i],cartesian_product_damian$y[i])
}

ggplot(cartesian_product_damian)+
  xlab(" ")+ylab(" ")+
  geom_point(aes(x,y, color=factor(lista)))+
  scale_color_manual(" ",breaks = c("-1", "1"),
                     values=c("red", "blue"))




  
  