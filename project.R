
#Wczytanie bibliotek 

library(rgdal)
library(ggplot2)
library(dbscan)
library(devtools)

#Wczytanie danych

dane<-readOGR(dsn="D:/Studia Materia³y/Semestr5/Analiza Danych Przestrzennych/Æwiczenia/Projekt/Projekt/damian/damian/MyProject/zestaw2_XYTableToPoint_XYTab.shp")
osiedla<-readOGR(dsn="D:/Studia Materia³y/Semestr5/Analiza Danych Przestrzennych/Æwiczenia/Projekt/dane/dane/osiedla.shp")

#Wyswietlenie danych

dane_frame <- data.frame(dane)

map <- ggplot() + geom_polygon(data = osiedla, aes(x = long, y = lat, group = group),
  colour = "black", fill = "lightblue") + geom_point(data=dane_frame,aes(x=coords.x1,y=coords.x2),
      shape=20,size = 2) + labs(title="Criminality") + 
            theme( panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
                               
map


#DBSCAN

db300_5 = dbscan(dane_frame, eps = 300, minPts = 5 )
db500_5 = dbscan(dane_frame, eps = 500, minPts = 5 )
db300_30 = dbscan(dane_frame, eps = 300, minPts = 30 )
db500_30 = dbscan(dane_frame, eps = 500, minPts = 30 )
db400_10 = dbscan(dane_frame, eps = 400, minPts = 10 )
db700_30 = dbscan(dane_frame, eps = 700, minPts = 30 )


map <- ggplot() + geom_polygon(data = osiedla, aes(x = long, y = lat, group = group),
  colour = "black", fill = "lightblue") + geom_point(data=dane_frame,aes(x=coords.x1,y=coords.x2),
  shape=20,size = 2,color=db500_30$cluster+1) + labs(title="Criminality") + 
  theme( panel.grid = element_blank(),
         axis.title = element_blank(),
         axis.text = element_blank(),
         axis.ticks = element_blank())
map

#HDBSCAN

hdb_5<-hdbscan(dane_frame,minPts = 5)
hdb_10<-hdbscan(dane_frame,minPts = 40)
hdb_20<-hdbscan(dane_frame,minPts = 20)
hdb_50<-hdbscan(dane_frame,minPts = 50)

map <- ggplot() + geom_polygon(data = osiedla, aes(x = long, y = lat, group = group),
    colour = "black", fill = "lightblue") + geom_point(data=dane_frame,aes(x=coords.x1,y=coords.x2),
    shape=20,size = 2,color=hdb_10$cluster+1) + labs(title="Criminality") + 
  theme( panel.grid = element_blank(),
         axis.title = element_blank(),
         axis.text = element_blank(),
         axis.ticks = element_blank())
map

#OPTICS

opt_005<-optics(dane_frame, eps = 500, minPts = 5 )
opt_005<-extractXi(opt_005,0.05)

opt_002<-optics(dane_frame, eps = 400, minPts = 20 )
opt_002<-extractXi(opt_002,0.02)

opt_xd<-optics(dane_frame, eps = 400, minPts = 10 )
opt_xd<-extractXi(opt_xd,0.05)


map <- ggplot() + geom_polygon(data = osiedla, aes(x = long, y = lat, group = group),
       colour = "black", fill = "lightblue") + geom_point(data=dane_frame,aes(x=coords.x1,y=coords.x2),
       shape=20,size = 2,color=opt_xd$cluster+1) + labs(title="Criminality") + 
  theme( panel.grid = element_blank(),
         axis.title = element_blank(),
         axis.text = element_blank(),
         axis.ticks = element_blank())
map






