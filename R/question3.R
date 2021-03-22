knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(tidymodels)
library(readxl)
library(neuralnet)
library(knitr)
library(quantmod)
library(e1071)
library("hydroGOF")

exchangeGBP <- read_excel("Documents/IIT-DMML/R/exchangeGBP.xlsx")
exchangeGBP <- janitor::clean_names(exchangeGBP)
exchangeGBP <- mutate(exchangeGBP, date_in_ymd = ymd(yyyy_mm_dd))
exchangeGBP <- select(exchangeGBP, -1)
exchangeGBP <- select(exchangeGBP, date_in_ymd, everything())


#all the input is in only one dataframe to be able to preserve the testing and training
#dataset for the two sets of input variables
#creating a new feature by using previous date values
gbp_exchange_full <- exchangeGBP
gbp_exchange_full <- mutate(gbp_exchange_full, lag1 = lag(exchangeGBP$gbp_eur,1))
gbp_exchange_full <- mutate(gbp_exchange_full, lag2 = lag(exchangeGBP$gbp_eur,2))
gbp_exchange_full <- mutate(gbp_exchange_full, lag3 = lag(exchangeGBP$gbp_eur,3))
gbp_exchange_full <- mutate(gbp_exchange_full, lag4 = lag(exchangeGBP$gbp_eur,4))
gbp_exchange_full <- mutate(gbp_exchange_full, lag5 = lag(exchangeGBP$gbp_eur,5))
gbp_exchange_full <- mutate(gbp_exchange_full, lag6 = lag(exchangeGBP$gbp_eur,6))
gbp_exchange_full <- mutate(gbp_exchange_full, lag7 = lag(exchangeGBP$gbp_eur,7))
gbp_exchange_full <- mutate(gbp_exchange_full, lag8 = lag(exchangeGBP$gbp_eur,8))
gbp_exchange_full <- mutate(gbp_exchange_full, five_day_rolling = rollmean(gbp_eur,5, fill = NA))
gbp_exchange_full <- mutate(gbp_exchange_full, ten_day_rolling = rollmean(gbp_eur,10, fill = NA))

gbp_exchange_full <- drop_na(gbp_exchange_full)  
gbp_exchange_full
summary(gbp_exchange_full)

#============== SLR Analysis ========================================
#Let's plot two input vectors to see if there are any relationship
## Prepare scatter plot
#Scatter Plot
plot(gbp_exchange_full[c("gbp_eur", "lag1")], pch=16)


# Analyze the simple liner regression model
# Create a linear regression model
## Add best-fit line to the scatter plot
#Fit linear model using OLS
model <- lm(lag1 ~ gbp_eur, gbp_exchange_full)

#Overlay best-fit line on scatter plot
# Add the fitted line
abline(model)
model

#Predict
# make a prediction for each X
predictedLag1 <- predict(model, gbp_exchange_full)
predictedLag1
# display the predictions
points(gbp_exchange_full$gbp_eur, predictedLag1, col = "blue", pch=4)
#Overlay Predictions on Scatter Plot
#Calculate RMSE 
RMSE=rmse(predictedLag1,gbp_exchange_full$lag1)
RMSE

#================= SVR Analysisi =========================
  
#Plot the data
plot(gbp_exchange_full[c("gbp_eur", "lag1")], pch=16)
#Regression with SVM
#We can try with difference kernal function
modelsvm <- svm(lag1~gbp_eur,data=gbp_exchange_full)
modelsvm <- svm(lag1~gbp_eur,data=gbp_exchange_full,kernel='linear', cost=10, epsilion=0.1)
modelsvm <- svm(lag1~gbp_eur,data=gbp_exchange_full,kernel='polynomial', cost=10, epsilion=0.1)
modelsvm <- svm(lag1~gbp_eur,data=gbp_exchange_full,kernel='sigmoid', cost=10, epsilion=0.1)
modelsvm <- svm(lag1~gbp_eur,data=gbp_exchange_full,kernel='radial', cost=10, epsilion=0.1)


#Predict using SVM regression
predYsvm = predict(modelsvm, gbp_exchange_full)

#Overlay SVM Predictions on Scatter Plot
points(gbp_exchange_full$gbp_eur, predYsvm, col = "red", pch=16)

## RMSE for SVR Model

#Calculate RMSE 
RMSEsvm=rmse(predYsvm,gbp_exchange_full$lag1)
RMSEsvm

## Tuning SVR model by varying values of maximum allowable error and cost parameter

#Tune the SVM model
OptModelsvm=tune(svm, lag1~gbp_eur, data=gbp_exchange_full,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))

#Print optimum value of parameters
print(OptModelsvm)

#Plot the perfrormance of SVM Regression model
plot(OptModelsvm)
#==========================================================

#In case, if we want to use two data set without normalizing
gbp_train_original <- gbp_exchange_full[1:400,]
gbp_test_original  <- gbp_exchange_full[401:491,]

# Get the min and max of the original training values
gbp_min_train <- min(gbp_exchange_full[1:400,2])
gbp_max_train <- max(gbp_exchange_full[1:400,2])

# Get the min and max of the original testing values
gbp_min_test <- min(gbp_exchange_full[401:491,2])
gbp_max_test <- max(gbp_exchange_full[401:491,2])

#Normalize the data set before applying into the model
# All the variables are normalized
normalized_gbp <- gbp_exchange_full
normalized_gbp <- mutate(normalized_gbp, across(2:12, ~normalize(.x)))

#Create two data sets by using normalized data for traning and testing
#Break the data set into two parts for traning
#Global variables which is applicable to the full scope of the program
set.seed(3000)
gbp_train <- normalized_gbp[1:400,]
gbp_test  <- normalized_gbp[401:491,]
gbp_train
gbp_test
# Look at the data that has been normalized
normalized_gbp
summary(normalized_gbp)

# Analyze the simple liner regression model
plot(gbp_train[c("gbp_eur", "lag1")], pch=16)
# Create a linear regression model
model <- lm(gbp_eur ~ lag2, gbp_train)
# Add the fitted line
abline(model)
model

#Predict
# make a prediction for each X
predictedGBP <- predict(model, gbp_train$lag2)
# display the predictions
points(gbp_train$gbp_eur, predictedGBP, col = "blue", pch=4)

#Apply SVR model

svmt <- svm(gbp_eur~lag1, data=gbp_train,kernel='linear', cost=10, epsilion=0.1)
svmt

svmtfv <- predict(svmt, data=gbp_train$lag1)
svmtfv

abline(svmt)
points(gbp_train$gbp_eur, svmtfv, col = "blue", pch=4)

plot(y=coredata(gbp_train$gbp_eur), x=coredata(gbp_train$lag1));
points(y=svmtfv, x=coredata(gbp_train$lag1), col="red")

result1 <- relevant_pred_stat(gbp_train$lag1,svmtfv, "Model1")
result1 <- filter(result1, .metric != "rsq")
all_result <- bind_rows(result1)

all_result <- janitor::clean_names(all_result)
all_result <- select(all_result, -estimator)
all_result <- pivot_wider(all_result, names_from = metric, values_from = estimate)
all_result <- arrange(all_result, rmse)
print(all_result)

#RMSE calculation
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# We can create a function to normalize the data from 0 to 1
# If we use leania activation function like 'logistic'
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}

# We can create a function to unnormalize the data=
unnormalize <- function(x, min, max) {
  return( (max - min)*x + min ) 
}

# Get the final statistics by comparing test data with out put result of NN
# metrices() function returns the statistics output like RMSE,MAE,MAPE
# @param 'true_value' data column from the testing data set
# @param 'predicted_value' data column from the resullt. 
relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}

# Combine the dataframes
set_a_models = rbind(set_a_models_l,set_a_models_two_layers)