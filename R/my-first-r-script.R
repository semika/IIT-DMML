

#How to write a function
pow <- function(base, exponent) {
  result <- base^exponent
  return(result)
}

pow(10, 2)

# How to write a function end

# List all the objects
objects()
ls()


#Vectors
x <- c(10,20,30,40,50,60,70,80,90,100)
x[0]
x[1:4]

#Identify data type

class(0.341)
class(TRUE)

#Arithmetic operators
x <- (22+33)/2 #Parannthesis first, Addition second, divition third
x

#Rational Operators
# >, <, >=, <=, ==, !=

############ Reading a .csv file ##################

employee <- read.csv("/Users/admin/Documents/IIT-DMML/R/employee.csv", header=TRUE)

#set workig directory
setwd("/Users/admin/Documents/IIT-DMML/R")
#use relative paths to read data
employee <- read.csv("employee.csv", header=TRUE)
?head()
head(employee)  # head() function will print first set of records in the file
print(employee$Name)

#get all software architects
softwareArchitect <- employee[employee$Designation == 'Software Architect',]
print(softwareArchitect)

#Can print only selected rows and columns
employee[1:2, 1:2]

#Can add new columns to data set
employee$Departmet <- "Tech"
print(employee)

#Also can remove columns
employee <- employee[, -6]
print(employee)

str(employee)

#Write CSV
?write.csv(employee)
write.csv(employee$Name, file="employee-out.csv")

############# Vectors #######################
?c()
animalVector <- c('cat','dog', 'parat', 'rabit', 'dear')
weightVector <- c(23,45,34,NA,56)
print(animalVector)

#print only a one value
print(animalVector[1])

#print range of values
print(animalVector[1:3])

#can apply conditions to whole vector at once
animalVector == 'cat'
length(animalVector)

#replace missing values
weightVector
?is.na()
weightVector[is.na(weightVector)] <- 45
weightVector

#can apply function to a vector
#get the mean of weights
?mean()
mean(weightVector)

#sort vector values
?sort()
sort(animalVector, decreasing = FALSE)

no <- 1:5
print(no)

#Can create data frames using two vectors
?data.frame
animalDataFrame <- data.frame(animalVector, weightVector, no)
print(animalDataFrame)

str(animalDataFrame)

###################### Installing a Package ################
install.packages("rpart")

#Load the library
library(rpart)

help(package="rpart")
?rpart

################### Control statemets ##############

#for loop, if, else
employee
