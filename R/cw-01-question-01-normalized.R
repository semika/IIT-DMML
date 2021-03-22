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

myzScore = function(x) {
  return((x - mean(x)) / sd(x))
}
normalize <- function(x, new_max = 1,new_min = 0) { # see how we define the max min values
  a = ( ((x-min(x)) * (new_max-new_min)) / (max(x)-min(x)) ) + new_min
  return(a)
}

vehicles <- read_excel("Documents/IIT-DMML/R/vehicles.xlsx")

#Remove classification field from origin data set.
vehicles_features <- vehicles
vehicles_features$Class <- NULL

#Normalized data set
#vehicles_features_normalized <- as.data.frame(lapply(vehicles_features, normalize))
#vehicles_features_normalized
vehicles_features_normalized <- as.data.frame(lapply(vehicles_features, myzScore))
vehicles_features_normalized

plot(vehicles_features_normalized[c("Comp", "Max.L.Ra")])
plot(vehicles_features_normalized[c("Sc.Var.Maxis", "Sc.Var.maxis")])

km = kmeans(vehicles_features_normalized, centers = 2, nstart = 25)
#Plot the given data as it is with class
plot(vehicles_features_normalized[c("Sc.Var.Maxis", "Sc.Var.maxis")], col=km$cluster)
points(km$centers[,c("Sc.Var.Maxis", "Sc.Var.maxis")], col=1:2, pch=23, cex=3)

vehicles$Class[vehicles$Class == 'bus'] <- 1
vehicles$Class[vehicles$Class == 'van'] <- 2
vehicles$Class[vehicles$Class == 'opel'] <- 3
vehicles$Class[vehicles$Class == 'saab'] <- 3

#Check confusion matrix
table(vehicles$Class, km$cluster)