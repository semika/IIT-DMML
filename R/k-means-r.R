library(datasets)
head(iris)
names(iris)

#Remove the output column
x <- iris[, -5]
head(x)

y <- iris$Species
head(y)

#create cluster
kc <- kmeans(x, 3)

kc

table(y,kc$cluster)  

plot(x[c("Sepal.Length", "Sepal.Width")], col=kc$cluster)

points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=23, cex=3)
