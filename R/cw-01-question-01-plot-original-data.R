vehicles <- read_excel("Documents/IIT-DMML/R/vehicles.xlsx")
#print(vehicles)

#Replace, bus = 'red', van = 'blue', opel = 'green', saab = 'black'
vehicles$Class[vehicles$Class == 'bus']  <- 'red'
vehicles$Class[vehicles$Class == 'van']  <- 'blue'
vehicles$Class[vehicles$Class == 'opel'] <- 'green'
vehicles$Class[vehicles$Class == 'saab'] <- 'black'


#Plot the original data set
#Plot the given data as it is with class
#plot(vehicles[c("Sc.Var.Maxis", "Sc.Var.maxis")], col=vehicles$Class, main="Over Given Classification Label")
#plot(vehicles[c("Comp", "Max.L.Ra")], col=vehicles$Class, main="Over Given Classification Label")

par(mfrow=(c(2, 2)))
plot(vehicles[c("Comp", "Max.L.Ra")], col=vehicles$Class, main="Graph1 :Over Given Label")
plot(vehicles[c("Sc.Var.Maxis", "Skew.maxis")], col=vehicles$Class, main="Graph2 :Over Given Label")
plot(vehicles[c("Skew.Maxis", "Skew.maxis")], col=vehicles$Class, main="Graph3 :Over Given Label")
plot(vehicles[c("Max.L.Ra", "Sc.Var.maxis")], col=vehicles$Class, main="Graph4 :Over Given Label")




# Z-core standization
#Remove classification field from origin data set.
vehicles_features <- vehicles
vehicles_features$Class <- NULL
vehicles_features_normalized <- as.data.frame(scale(vehicles_features))
#vehicles_features_normalized <- as.data.frame(lapply(vehicles_features, myzScore))
vehicles_features_normalized["Class"] <- vehicles$Class
vehicles_features_normalized

par(mfrow=(c(2, 2)))
plot(vehicles_features_normalized[c("Comp", "Max.L.Ra")], col=vehicles_features_normalized$Class, main="Graph1 :Over Given Label")
plot(vehicles_features_normalized[c("Sc.Var.Maxis", "Skew.maxis")], col=vehicles_features_normalized$Class, main="Graph2 :Over Given Label")
plot(vehicles_features_normalized[c("Skew.Maxis", "Skew.maxis")], col=vehicles_features_normalized$Class, main="Graph3 :Over Given Label")
plot(vehicles_features_normalized[c("Max.L.Ra", "Sc.Var.maxis")], col=vehicles_features_normalized$Class, main="Graph4 :Over Given Label")
