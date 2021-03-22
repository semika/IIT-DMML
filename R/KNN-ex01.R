# ============================== R code ==============================
# loading packages
library(ggplot2)
library(magrittr)
library(class) # KNN

# sepal width vs. sepal length
iris %>%
  ggplot(aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_point()

# petal width vs. petal length
iris %>%
  ggplot(aes(x=Petal.Length, y=Petal.Width, color=Species)) +
  geom_point()

# Summary overview of `iris`
summary(iris) 

# Refined summary overview
summary(iris[c("Petal.Width", "Sepal.Width")])

# Normalize the `iris` data
iris_norm <- as.data.frame(lapply(iris[1:4], normalize))
# Summarize `iris_norm`
summary(iris_norm)

set.seed(1234)

ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))

# Compose training set
iris.training <- iris[ind==1, 1:4]

# Inspect training set
head(iris.training)
nrow(iris.training)

# Compose test set
iris.test <- iris[ind==2, 1:4]

# Inspect test set
head(iris.test)
nrow(iris.test)

# Compose `iris` training labels
iris.trainLabels <- iris[ind==1,5]

# Inspect result
print(iris.trainLabels)

# Compose `iris` test labels
iris.testLabels <- iris[ind==2, 5]

# Inspect result
print(iris.testLabels)

# Build the model
iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)
# Inspect `iris_pred`
iris_pred

#KNN Evaluation

# Put `iris.testLabels` in a data frame
irisTestLabels <- data.frame(iris.testLabels)
# Merge `iris_pred` and `iris.testLabels` 
merge <- data.frame(iris_pred, iris.testLabels)
merge
# Specify column names for `merge`
names(merge) <- c("Predicted Species", "Observed Species")
# Inspect `merge` 
merge
# Confusion Matix
table(iris_pred,iris.testLabels)



# Build your own `normalize()` function
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}




