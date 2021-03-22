library(datasets)
head(iris)
set.seed(20)

irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
irisCluster
