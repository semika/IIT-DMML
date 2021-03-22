knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(tidymodels)
library(readxl)
library(neuralnet)
library(knitr)

exchangeGBP <- read_excel("Documents/IIT-DMML/R/exchangeGBP.xlsx") %>%
  janitor::clean_names() %>%
  mutate(date_in_ymd = ymd(yyyy_mm_dd)) %>%
  select(-1) %>%
  select(date_in_ymd,everything())


#all the input is in only one dataframe to be able to preserve the testing and training
#dataset for the two sets of input variables
gbp_exchange_full = exchangeGBP %>%
  mutate(previous_one_day_set_a = lag(exchangeGBP$gbp_eur,1),
         previous_one_day_set_b = lag(exchangeGBP$gbp_eur,1),
         previous_two_day_set_b = lag(exchangeGBP$gbp_eur,2),
         previous_one_day_set_c = lag(exchangeGBP$gbp_eur,1),
         previous_two_day_set_c = lag(exchangeGBP$gbp_eur,2),
         previous_three_day_set_c = lag(exchangeGBP$gbp_eur,3),
         previous_one_day_set_d = lag(exchangeGBP$gbp_eur,1),
         previous_two_day_set_d = lag(exchangeGBP$gbp_eur,2),
         five_day_rolling = rollmean(gbp_eur,5, fill = NA),
         ten_day_rolling = rollmean(gbp_eur,10, fill = NA)) %>%
  
  
  drop_na()

gbp_exchange_full$five_day_rolling

gbp_exchange_full %>%
  pivot_longer(cols = 3,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) + geom_line() + facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  labs(x = "", title = "First Set of Input Variables") +
  theme(legend.position = "none")


gbp_exchange_full %>%
  pivot_longer(cols = c(4,5),names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Second Set of Input Variables") +
  theme(legend.position = "none")

gbp_exchange_full %>%
  pivot_longer(cols = 6:8,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Third Set of Input Variables") +
  theme(legend.position = "none")

gbp_exchange_full %>%
  pivot_longer(cols = 9:12,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Fourth Set of Input Variables") +
  theme(legend.position = "none")

# We can create a function to normalize the data from 0 to 1
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
# All the variables are normalized
normalized_gbp = gbp_exchange_full %>%
  mutate(across(2:12, ~normalize(.x)))
# Look at the data that has been normalized
normalized_gbp
summary(normalized_gbp)

set.seed(123)
gbp_train <- normalized_gbp[1:400,]
gbp_test <- normalized_gbp[401:491,]

# We can create a function to unnormalize the data=
unnormalize <- function(x, min, max) {
  return( (max - min)*x + min ) }
# Get the min and max of the original training values
gbp_min_train <- min(gbp_exchange_full[1:400,2])
gbp_max_train <- max(gbp_exchange_full[1:400,2])
# Get the min and max of the original testing values
gbp_min_test <- min(gbp_exchange_full[401:491,2])
gbp_max_test <- max(gbp_exchange_full[401:491,2])
# Check the range of the min and max of the training dataset
gbp_min_test

gbp_min_train

gbp_max_test
gbp_max_train
relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}



relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}





relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}



set.seed(12345)
# function setup that creates 2 layer model
model_two_hidden_layers = function(hidden,sec_hidden) {
  nn_model_true = neuralnet(gbp_eur ~ previous_one_day_set_a, data=gbp_train, hidden=c(hidden,sec_hidden), linear.output=TRUE)
  train_results = compute(nn_model_true, gbp_test[,2:3])
  truthcol = gbp_exchange_full[401:491,2]$gbp_eur
  predcol = unnormalize(train_results$net.result, gbp_min_train, gbp_max_train)[,1]
  relevant_pred_stat(truthcol,predcol,
                     "Two Hidden Layers") %>%
    mutate(hiddel_layers = paste0(hidden, " and ",sec_hidden),
           input_set = "A") %>%
    filter(.metric != "rsq")
}


# creation of different models with varying number of nodes
results_two_hidden_layers = bind_rows(
  lapply(1:10, function(n) {
    bind_rows(
      lapply(1:5, function(m) {
        model_two_hidden_layers(n,m)
      })
    )
  })) %>%
  janitor::clean_names()
# save the stat indices to a dataframe
set_a_models_two_layers = results_two_hidden_layers %>%
  select(-estimator) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
  arrange(rmse)
kable(set_a_models_two_layers[1:10,])



# Combine the dataframes
set_a_models = rbind(set_a_models_l,set_a_models_two_layers)









