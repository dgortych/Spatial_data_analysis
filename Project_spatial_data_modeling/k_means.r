

data<- read.csv('DamianG2t_output_0.txt',sep=';',dec=".")

data <- data[0:25]   #dla danych D

data <- data[ data$Dip_dir != 'x', ]   #dla danych W

group_1<-data[,c('X_N','Y_N','Z_N')]  

cl_1 <-lapply(2:5, function(i) kmeans(group_1,i))

table(cl_1[[1]]$cluster)

group_2<-data[,c('X_D','Y_D','Z_D')]

cl_2 <-lapply(2:5, function(i) kmeans(group_2,i))

library(dplyr)
library(ggplot2)

for(i in 1:4){
  data<-mutate(data,color=cl_1[[i]]$cluster)
  filename<- paste("cl_1_", i+1, ".png", sep = "")
  obj<-ggplot(data,aes(x=Y_C,y=X_C,color=factor(color)))+geom_point(size=0.05)
  obj<- obj + guides(color = guide_legend(override.aes = list(size = 3)))
  png(filename)
  print(obj)
  dev.off()
}

for(i in 1:4){
  data<-mutate(data,color=cl_2[[i]]$cluster)
  filename<- paste("cl_2_", i+1, ".png", sep = "")
  obj<-ggplot(data,aes(x=Y_C,y=X_C,color=factor(color)))+geom_point(size=0.05)
  obj<- obj + guides(color = guide_legend(override.aes = list(size = 3)))
  png(filename)
  print(obj)
  dev.off()
}


obj