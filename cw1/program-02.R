vehicles <- read_excel("Documents/IIT-DMML/R/vehicles.xlsx")
#print(vehicles)

#Replace, bus = 'red', van = 'blue', opel = 'green', saab = 'black'
vehicles$Class[vehicles$Class == 'bus']  <- 'red'
vehicles$Class[vehicles$Class == 'van']  <- 'blue'
vehicles$Class[vehicles$Class == 'opel'] <- 'green'
vehicles$Class[vehicles$Class == 'saab'] <- 'black'

# Saclig data set
#Remove classification field from original data set before scaling data.
vehicles_features <- vehicles
vehicles_features$Class <- NULL
vehicles_features_normalized <- as.data.frame(scale(vehicles_features))
#Append the 'Class' field into the scaled data set again
vehicles_features_normalized["Class"] <- vehicles$Class

par(mfrow=(c(2, 2)))
plot(vehicles_features_normalized[c("Comp", "Max.L.Ra")], col=vehicles_features_normalized$Class, main="Graph1 :Over Given Label")
plot(vehicles_features_normalized[c("Sc.Var.Maxis", "Skew.maxis")], col=vehicles_features_normalized$Class, main="Graph2 :Over Given Label")
plot(vehicles_features_normalized[c("Skew.Maxis", "Skew.maxis")], col=vehicles_features_normalized$Class, main="Graph3 :Over Given Label")
plot(vehicles_features_normalized[c("Max.L.Ra", "Sc.Var.maxis")], col=vehicles_features_normalized$Class, main="Graph4 :Over Given Label")
