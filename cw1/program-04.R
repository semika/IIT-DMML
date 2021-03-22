library(tidyverse)
library(readxl)
library(NbClust)
library(knitr)
library(tidymodels)
library(flexclust)
library(funtimes)
library(gridExtra)
theme_set(theme_light())

# Read in the original excel datafile
vehicles <- read_excel("Documents/IIT-DMML/R/vehicles.xlsx")
vehicles_processed <- janitor::clean_names(vehicles)
vehicles_processed <- mutate(vehicles_processed, class = as_factor(class))
summary(vehicles_processed)
print(vehicles_processed)

# Replace text labels with numeric ones
# This is needed later in order to get 'confusion' matrix
# To cross check the cluster results with actual data set.
vehicles_with_numeric_label <- vehicles
vehicles_with_numeric_label$Class[vehicles_with_numeric_label$Class == 'bus']  <- 1
vehicles_with_numeric_label$Class[vehicles_with_numeric_label$Class == 'van']  <- 2
vehicles_with_numeric_label$Class[vehicles_with_numeric_label$Class == 'opel'] <- 3
vehicles_with_numeric_label$Class[vehicles_with_numeric_label$Class == 'saab'] <- 4

#Remove outliers of 'bus'
vehicles_bus_quantiled = vehicles_processed
vehicles_bus_quantiled <- filter(vehicles_bus_quantiled, class == "bus")
vehicles_bus_quantiled <- mutate(vehicles_bus_quantiled, across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))
print(vehicles_bus_quantiled)

#Remove outliers of 'van'
vehicles_van_quantiled = vehicles_processed
vehicles_van_quantiled <- filter(vehicles_van_quantiled, class == "van")
vehicles_van_quantiled <- mutate(vehicles_van_quantiled, across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))
print(vehicles_van_quantiled)

#Remove outliers of 'opel'
vehicles_opel_quantiled = vehicles_processed
vehicles_opel_quantiled <- filter(vehicles_opel_quantiled, class == "opel")
vehicles_opel_quantiled <- mutate(vehicles_opel_quantiled, across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))
print(vehicles_opel_quantiled)

#Remove outliers of 'saab'
vehicles_saab_quantiled = vehicles_processed
vehicles_saab_quantiled <- filter(vehicles_saab_quantiled, class == "saab")
vehicles_saab_quantiled <- mutate(vehicles_saab_quantiled, across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))
print(vehicles_saab_quantiled)

#Combined again the 4 data set into one
vehicle_quantiled = bind_rows(list(vehicles_bus_quantiled,vehicles_van_quantiled,vehicles_opel_quantiled,vehicles_saab_quantiled))
vehicle_quantiled <-  arrange(vehicle_quantiled, samples)
summary(vehicle_quantiled)


#Plot quantile data 'bus'  
vehicle_quantiled_bus <- pivot_longer(vehicle_quantiled, 2:19,names_to = "labels")
vehicle_quantiled_bus <- filter(vehicle_quantiled_bus, class == "bus")
vehicle_quantiled_bus <- mutate(vehicle_quantiled_bus, class = fct_reorder(class,value,median))
ggplot(vehicle_quantiled_bus, aes(class, value, fill = reorder(labels,value))) + geom_boxplot() + labs(title = "Transformed Outliers class: 'bus'")

#Plot quantile data 'saab'  
vehicle_quantiled_saab <- pivot_longer(vehicle_quantiled, 2:19,names_to = "labels")
vehicle_quantiled_saab <- filter(vehicle_quantiled_saab, class == "saab")
vehicle_quantiled_saab <- mutate(vehicle_quantiled_saab, class = fct_reorder(class,value,median))
ggplot(vehicle_quantiled_saab, aes(class, value, fill = reorder(labels,value))) + geom_boxplot() + labs(title = "Transformed Outliers class: 'saab'")

#Plot quantile data 'opel'  
vehicle_quantiled_opel <- pivot_longer(vehicle_quantiled, 2:19,names_to = "labels")
vehicle_quantiled_opel <- filter(vehicle_quantiled_opel, class == "opel")
vehicle_quantiled_opel <- mutate(vehicle_quantiled_opel, class = fct_reorder(class,value,median))
ggplot(vehicle_quantiled_opel, aes(class, value, fill = reorder(labels,value))) + geom_boxplot() + labs(title = "Transformed Outliers class: 'opel'")

#Plot quantile data 'van'  
vehicle_quantiled_van <- pivot_longer(vehicle_quantiled, 2:19,names_to = "labels")
vehicle_quantiled_van <- filter(vehicle_quantiled_van, class == "van")
vehicle_quantiled_van <- mutate(vehicle_quantiled_van, class = fct_reorder(class,value,median))
ggplot(vehicle_quantiled_van, aes(class, value, fill = reorder(labels,value))) + geom_boxplot() + labs(title = "Transformed Outliers class: 'van'")

#Normalized the data set.
vehicle_quantiled_scaled <- vehicle_quantiled
vehicle_quantiled_scaled$samples <- NULL
vehicle_quantiled_scaled$class <- NULL
vehicle_quantiled_scaled <- mutate(vehicle_quantiled_scaled, across(everything(), scale))
print(vehicle_quantiled_scaled)


#Determine the number of cluster
set.seed(123)
# Perform the kmeans using the NbClust function
# Use Euclidean for distance
cluster_euclidean = NbClust(vehicle_quantiled_scaled,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")

# Use manhattan for distance
cluster_manhattan = NbClust(vehicle_quantiled_scaled,distance="manhattan", min.nc=2,max.nc=15,method="kmeans",index="all")


#Apply K-Means
km = kmeans(vehicle_quantiled_scaled, centers = 2, nstart = 25)
km
#Plot the given data as it is with class
plot(vehicle_quantiled_scaled[c("sc_var_maxis", "sc_var_maxis_2")], col=km$cluster)
points(km$centers[,c("sc_var_maxis", "sc_var_maxis_2")], col=1:2, pch=23, cex=2)

#compare cluster results with original data
table(vehicles_with_numeric_label$Class,km$cluster)  










vehicles_with_numeric_label['cluster'] <- km$cluster
vehicles_with_numeric_label['Class', 'cluster']



#Plot two features against the cluster outcome
plot(vehicle_quantiled[c("sc_var_maxis", "sc_var_maxis_2")], col=km$cluster)



