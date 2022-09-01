
#�aadowanie bibliotek
library(ncdf4)
library(fields)
library(raster) #do utworzenia obiektu rasterbrick
library(dplyr) #filtrowanie ramek danych
library(sp)
library(rgdal)
library(terra) #do budowania rastra, alternatywa pakietu raster
library(purrr)
library(magrittr)
library(tidyr)

#Otwarcie pliku
nc<- nc_open("gebco_2021_n54.7393798828125_s53.6572265625_w15.2215576171875_e16.710205078125.nc")

#Odczytanie struktury pliku
print(nc)

#Pobranie danych i zapisanie do zmiennych
elev <- ncvar_get(nc, "elevation")
lon <- ncvar_get(nc, "lon") #praktyka nietypowa - to nie jest zmienna
lat <- ncvar_get(nc, "lat") #praktyka nietypowa - to nie jest zmienna

#Sprawdzenie rozmiaru danych
length(lon)
length(lat)
length (elev)

#Test wymiaru
length(elev)==length(lon)*length(lat)

#Sprawdzenie typu danych
class(lon)
class(lat)
class(elev)

#Sposób 2. "Półautomatyczne" utworzenie tabeli ze współrzędnymi XYZ. Wykorzystanie funkcji expand.grid - 2-3 sekundy.
#Uwaga sprawdź jak jest indeksowany elev: elev[dimension,dimension]
cartesian_product<- expand.grid(lon,lat) cartesian_product<- expand.grid(lat,lon) #Czy te dwie wersje są przemienne?
class(cartesian_product)

#Sposób 2. Wstawienie nowego elementu i wyświetlenie
cartesian_product<- dplyr::mutate(cartesian_product, c(elev) )
colnames(cartesian_product)<-c("x","y", "z")
head(cartesian_product)

#Wskazanie współrzędnych - pakiet sp
coordinates(cartesian_product) <- c("x", "y") 

#Nadanie układu współrzędnych geograficznych
proj4string(cartesian_product) <- CRS("+proj=longlat +datum=WGS84")

#Nadanie układu współrzędnych prostokątnych (układ UTM - sprawdź w Internecie swoją strefę UTM)
cartesian_product <- spTransform(cartesian_product, CRS("+proj=utm +zone=33 ellps=WGS84"))
cartesian_product<-as.data.frame(cartesian_product)
cartesian_product<-dplyr::mutate(cartesian_product, id=1:nrow(cartesian_product))
head(cartesian_product)
nrow(cartesian_product)

#Wybór kolumn w kolejności i zapisanie do pliku
#Uwaga: w normalnym projekcie to obojętne ale w naszym projekcie w
#pierwszej kolumnie powinna być szerokość geograficzna, potem długość, potem wysokość, potem id
cartesian_product<-dplyr::select(cartesian_product, c(2,3,1,4))
head(cartesian_product)
write.table(x=cartesian_product, file = "cartesian_productWiktorpop.csv", sep=" ", row.names = F, col.names = F )

#zamknięcie pliku
nc_close(nc)



