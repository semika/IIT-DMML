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
#summary(vehicles)
vehicles_processed <- janitor::clean_names(vehicles)

#mutate() method is used to create new variables from exsisting data set.
#this comes under 'dplyr' package which is a part of 'tidyverse'
# 'dplyr' provides function for selecting, filtering, grouping and arrnging data
vehicles_processed <- mutate(vehicles_processed, class = as_factor(class))
#print(vehicles_processed)
# Get a birds eye view of how the dataset looks like

#vehicles_processed_bus_normalized <- vehicles_processed_bus
#vehicles_processed$class <- NULL
#vehicles_processed <- as.data.frame(scale(vehicles_processed))
#vehicles_features_normalized <- as.data.frame(lapply(vehicles_features, myzScore))
#vehicles_processed["class"] <- vehicles$Class
#vehicles_processed

summary(vehicles_processed)

#Determine outliers for whole data set at once
vehicles_processed_all  <- pivot_longer(vehicles_processed,2:19,names_to = "labels")
#vehicles_processed_all  <- filter(vehicles_processed_all, class == "van")
vehicles_processed_all  <- mutate(vehicles_processed_all, class = fct_reorder(class,value,median))
ggplot(vehicles_processed_all, aes(class, value, fill = reorder(labels,value))) + geom_boxplot() + labs(title = "Outlier Detection for class: 'all'")

#Determine outliers for 'van'
vehicles_processed_van  <- pivot_longer(vehicles_processed,2:19,names_to = "labels")
vehicles_processed_van  <- filter(vehicles_processed_van, class == "van")
vehicles_processed_van  <- mutate(vehicles_processed_van, class = fct_reorder(class,value,median))
ggplot(vehicles_processed_van, aes(class, value, fill = reorder(labels,value))) + geom_boxplot() + labs(title = "Outlier Detection for class: 'van'")

#Determine outliers for 'bus'
vehicles_processed_bus  <- pivot_longer(vehicles_processed,2:19,names_to = "labels")
vehicles_processed_bus  <- filter(vehicles_processed_bus, class == "bus")
vehicles_processed_bus  <- mutate(vehicles_processed_bus, class = fct_reorder(class,value,median))
ggplot(vehicles_processed_bus, aes(class, value, fill = reorder(labels,value))) + geom_boxplot() + labs(title = "Outlier Detection for class: 'bus'")

#Determine outliers for 'saab'
vehicles_processed_saab  <- pivot_longer(vehicles_processed,2:19,names_to = "labels")
vehicles_processed_saab  <- filter(vehicles_processed_saab, class == "saab")
vehicles_processed_saab  <- mutate(vehicles_processed_saab, class = fct_reorder(class,value,median))
ggplot(vehicles_processed_saab, aes(class, value, fill = reorder(labels,value))) + geom_boxplot() + labs(title = "Outlier Detection for class: 'saab'")

#Determine outliers for 'opel'
vehicles_processed_opel  <- pivot_longer(vehicles_processed,2:19,names_to = "labels")
vehicles_processed_opel  <- filter(vehicles_processed_opel, class == "opel")
vehicles_processed_opel  <- mutate(vehicles_processed_opel, class = fct_reorder(class,value,median))
ggplot(vehicles_processed_opel, aes(class, value, fill = reorder(labels,value))) + geom_boxplot() + labs(title = "Outlier Detection for class: 'opel'")


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


# Remove the sample name and the class name. Both of these will be remove so that only n
#umerical data is left for the algorithm.
vehicles_data_points = combined %>%
  select(-samples, -class)
# Now that we have the "vehicles_data_points" dataset, scaling is performed
vehicles_scaled = vehicles_data_points %>%
  mutate(across(everything(), scale))

set.seed(123)
# Perform the kmeans using the NbClust function
# Use Euclidean for distance
cluster_euclidean = NbClust(vehicles_scaled,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")

# Use manhattan for distance
cluster_manhattan = NbClust(vehicles_scaled,distance="manhattan", min.nc=2,max.nc=15,method="kmeans",index="all")
