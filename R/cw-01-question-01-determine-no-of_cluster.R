vehicles <- read_excel("Documents/IIT-DMML/R/vehicles.xlsx")
vehicles_features <- vehicles
vehicles_features$Class <- NULL

k = 2:10
set.seed(42)	
WSS = sapply(k, function(k) {kmeans(vehicles_features, centers=k)$tot.withinss})

plot(k, WSS, type="l", xlab= "Number of k", ylab="Within sum of squares")
