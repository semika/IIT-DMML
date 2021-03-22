library(tidyverse)
library(readxl)
library(NbClust)
library(knitr)
library(tidymodels)
library(flexclust)
library(funtimes)
library(gridExtra)

# Read in the original excel datafile
vehicles <- read_excel("Documents/IIT-DMML/R/vehicles.xlsx")
summary(vehicles)
vehicles_processed <- janitor::clean_names(vehicles)
vehicles_processed <- mutate(vehicles_processed, class = as_factor(class))

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