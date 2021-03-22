knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(tidymodels)
library(readxl)
library(neuralnet)
library(knitr)

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

#Plot few input feature vectors to see any outlizers
gbp_exchange_full %>%
  pivot_longer(cols = 3:3,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) + geom_line() + facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + labs(x = "", title = "Lag1 Input Variables") + theme(legend.position = "none")

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

#Plot the same input feature vection which was plotted above again
#by using normalized data
#and show the difference
#there should not be any depection in the second plot for outliers.

normalized_gbp %>%
  pivot_longer(cols = 3:3,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) + geom_line() + facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +labs(x = "", title = "Lag1 Input Variables - After Normalized") + theme(legend.position = "none")

#Apply the MLP-ANN with random network architecture paramters.

result1 <- model_two_hidden_layers(2, 4, 'lag1')
result2 <- model_two_hidden_layers(6, 9, 'lag1')
result3 <- model_two_hidden_layers(5, 5)
result4 <- model_two_hidden_layers(12, 15)

#Combine all results
all_result <- bind_rows(result1)

all_result <- janitor::clean_names(all_result)
all_result <- select(all_result, -estimator)
all_result <- pivot_wider(all_result, names_from = metric, values_from = estimate)
all_result <- arrange(all_result, rmse)
print(all_result)

# creation of different models with varying number of nodes with two hidden layers
# This method will create 50 different network models and print the
# final result.
results_two_hidden_layers = bind_rows(
  lapply(1:10, function(n) {
    bind_rows(
      lapply(1:5, function(m) {
        model_two_hidden_layers(n,m,'lag2+lag2')
      })
    )
  })) %>%
  janitor::clean_names()
# save the stat indices to a dataframe
set_a_models_two_layers <- results_two_hidden_layers
set_a_models_two_layers <- select(results_two_hidden_layers, -estimator)
set_a_models_two_layers <- pivot_wider(set_a_models_two_layers, names_from = metric, values_from = estimate)
set_a_models_two_layers <- arrange(set_a_models_two_layers, rmse)
print(set_a_models_two_layers, n=50)

# function setup that creates 2 layer model
model_two_hidden_layers <- function(hidden,sec_hidden,input_set) {
  nn_model_true = neuralnet(gbp_eur~lag1, data=gbp_train, hidden=c(hidden,sec_hidden), linear.output=TRUE, act.fct = 'logistic') # tanh, relu, sigmoid,logistic
  #plot(nn_model_true)
  train_results = compute(nn_model_true, gbp_test[,2:4])
  
  # It is important to note that 'truthcol' is taken from the original un-normalized data set
  
  truthcol = gbp_exchange_full[401:491,2]$gbp_eur  
  
  #It is important to un-normalized the result before comparing with original data column
  #Otherwise, we tend to compare  normalized column with non-normalized column
  #Which may output incorrect results
  
  predcol = unnormalize(train_results$net.result, gbp_min_train, gbp_max_train)[,1] 
  
  #Print the results for lag1
  result <- relevant_pred_stat(truthcol, predcol, "Two Hidden Layers")
  result <- mutate(result, hiddel_layers = paste0(hidden, " and ", sec_hidden), input_set = input_set)
  result <- filter(result, .metric != "rsq")
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