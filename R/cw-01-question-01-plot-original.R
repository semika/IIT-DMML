install.packages("dplyr")
install.packages("stats")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("NbClust") 

library(readxl)
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(NbClust)


vehicles <- read_excel("Documents/IIT-DMML/R/vehicles.xlsx")

#Remove classification field from origin data set.
vehicles_features <- vehicles
vehicles_features$Class <- NULL

km = kmeans(vehicles_features, centers = 3, nstart = 25)
#Plot the given data as it is with class
plot(vehicles_features[c("Comp", "Max.L.Ra")], col=km$cluster)
points(km$centers[,c("Comp", "Max.L.Ra")], col=1:3, pch=23, cex=3)

#Check confusion matrix
table(vehicles$Class, km$cluster)

wssplot(selectedData) # suggested number of clusters is 3

set.seed(20)
#Determinign the appropriate number of clusters for the data set
clusterNo=NbClust(selectedData, distance="euclidean", min.nc=2,max.nc=4,method="kmeans",index="all")

km = kmeans(selectedData, centers = 2, nstart = 25)
#km = kmeans(selectedData[, 1:6], centers = 3, nstart = 25)
km
km$centers
km$cluster

#Compare K-means out put with Original data set class attribute
table(vehicles$Class, km$cluster)

#Plot the given data as it is with class
plot(selectedData[c("Comp", "Max.L.Ra")], col=km$cluster)
points(km$centers[,c("Comp", "Max.L.Ra")], col=1:2, pch=23, cex=3)

#plot(vehicles[c("Comp", "Max.L.Ra")], col='red')
