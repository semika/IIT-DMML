age <- c(25, 35, 50)
salary <- c(200000, 1200000, 2000000)
df <- data.frame( "Age" = age, "Salary" = salary, stringsAsFactors = FALSE)
df

plot(df[c("Age", "Salary")])
#Min-Max normalization
#Draw back, can have out liers.
#Tends to squeze the data towards the mean.
#If want to get out liers weighted, z-zcore standazization is better
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

new_normalize <- function(x, new_max = 1,new_min = 0) { # see how we define the max min values
  a = ( ((x-min(x)) * (new_max-new_min)) / (max(x)-min(x)) ) + new_min
  return(a)
}


myzScore = function(x) {
  return((x - mean(x)) / sd(x))
}


?lapply

#Apply fuction to each and every feature colum in the data frame
dfNorm <- as.data.frame(lapply(df, normalize))
dfNorm

#Can also specify only a selected columns
dfNorm <- as.data.frame(lapply(df[1:2], normalize))
dfNorm

#can specify even a single column
dfNormSalary <- as.data.frame(lapply(df[2], normalize))
dfNormSalary

#can apply the function only for specific column with column name
dfNormSalary <- as.data.frame(lapply(df["Salary"], normalize))
dfNormSalary

dfNorm1 <- as.data.frame(lapply(df[1:2], new_normalize))
dfNorm1

# Z-core standization 
dfNormZ <- as.data.frame( scale(df[1:2] ))
dfNormZ

plot(dfNormZ[c("Age", "Salary")])
plot(vehicles[c("Sc.Var.Maxis", "Sc.Var.maxis")])

dfNorm4 <- as.data.frame(lapply(df, myzScore))
dfNorm4



