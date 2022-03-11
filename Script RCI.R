#Preparation for the dashboard.
install.packages("ggplot2")

#Here begins the process for the wrangling of the data and creation of the datasets used.

require(tidyverse)
require(lubridate)
require(parsnip)
require(rsample)

rci_data <-  read.csv("https://www.dropbox.com/s/5qvwqyp55lkoviv/programming_test_data.csv?dl=1")


rci_data_filtered <- rci_data %>% count(local_sys_customer_id) %>% 
  filter(n > 1) %>% rename(contracts=n) 

#Here I am extracting all the unique identifiers
#of clients that have more than one contract with the company.

clients_index <- which(rci_data$local_sys_customer_id %in% rci_data_filtered$local_sys_customer_id)

#Here i am checking which are the rows in which there are clients that have more than one contract
#On the original dataframe.

clients_renovation <- rci_data[clients_index,] #Now I am only working with clients that have renovations, I used the row indexes to filter the main table.
clients_renovation$contract_start_date <- as.Date(clients_renovation$contract_start_date,"%Y-%m-%d")
clients_renovation$contract_planned_end_date <- as.Date(clients_renovation$contract_planned_end_date,"%Y-%m-%d")
clients_renovation$contract_real_end_date <- as.Date(clients_renovation$contract_real_end_date,"%Y-%m-%d")
#Some common wrangling, converting to date.

clients_renovation <- clients_renovation %>% arrange(local_sys_customer_id,desc(contract_start_date))
#Ordering data by client id and then by starting date on descending order.

x <- 1
time_last_contract <- c()

for (x in 1:137) {
  
time_differences <-  ifelse(clients_renovation[x,1] == clients_renovation[x+1,1], 
  clients_renovation[x,12] - clients_renovation[x+1,14],
  "")
time_last_contract <- append(time_last_contract,time_differences)

x <- x+1
  
}

clients_renovation_finished <- cbind(clients_renovation,time_last_contract)

#Here I already have the number of days since the last contract, for each client.
#A blank space is put into place when there is a change in client. Now is necessary
#Convert to months.

clients_renovation_finished$time_last_contract <- as.numeric(clients_renovation_finished$time_last_contract)
clients_renovation_finished[is.na(clients_renovation_finished$time_last_contract),25] <- 0
clients_renovation_finished[clients_renovation_finished$time_last_contract<0,25] <- 0
#I am replacing NA values for 0 because this means that there is no time before this contract and another one.
#Similarly, there are negative values when a client takes a new contract without having ended
#The previous one, I am turning those numbers also to 0.

clients_renovation_finished <- clients_renovation_finished %>% 
  mutate(months_last = round(time_last_contract/30.417, digit=0))

#And this transforms the days into months as asked. This ends point 1.

#Now, to get insights about the reasons behind a renovation I am goint to develop
#A simple logit model, for this, I will need o bind again this dataframe with 
#the original one.

clients_not_renovated <- rci_data[-clients_index,] #this has all the values that the dataframe with renovations dont have.

clients_not_renovated$time_last_contract <- 0
clients_not_renovated$months_last <- 0

clients_renovation_finished$renovate <- 1
clients_not_renovated$renovate <- 0 #creating dummy variables for the logit

renovation_merged <- rbind(clients_renovation_finished,clients_not_renovated) #set to go

renovation_merged$renovate <- as.factor(renovation_merged$renovate)
renovation_merged$dummy <- 1

renovation_merged$vehicle <- as.factor(renovation_merged$vehicle)
renovation_merged2 <- renovation_merged %>% drop_na(vehicle)  %>% spread(key = vehicle,value=dummy,fill = 0)

renovation_merged2$model_year <- as.factor(renovation_merged2$model_year)
renovation_merged2 <- renovation_merged2 %>% drop_na(model_year)  %>% mutate(dummy = 1) %>% spread(key = model_year,value=dummy,fill = 0)

renovation_merged2$object_quality <- as.factor(renovation_merged2$object_quality)

renovationformodel <- renovation_merged2[,c(6,9,10,14:17,19:23,26,28:43)]
renovationformodel$client_rate <- as.numeric(renovationformodel$client_rate)



fitted_model <- glm(renovate ~ .,family=binomial,data=renovationformodel)

summary(fitted_model)


#here are the results, but i will work with this dataframe in power bi to create visualizations.

write.csv(renovationformodel,"renovationformodel.csv")

stargazer::stargazer(fitted_model,type = "html",out = "modelo_logit_renovacion.html")

#I will only use statistically significant variables to ease interpretation.

#Finally, for the last point, I will do another simple logit model,
#Though this time I will split the dataset in training and testing, for 
#avoiding overfitting.

  rci_data2 <- subset(rci_data,contract_status != "CANCELADO")
  
  #I am keeping only the two outcomes of interest for this exercise.
  
  rci_data2$contract_start_date <- as.Date(rci_data2$contract_start_date,"%Y-%m-%d")
  rci_data2$contract_planned_end_date <- as.Date(rci_data2$contract_planned_end_date,"%Y-%m-%d")
  rci_data2$contract_real_end_date <- as.Date(rci_data2$contract_real_end_date,"%Y-%m-%d")
  
  rci_data2$actualdate_endingdate <- rci_data2$contract_real_end_date
  rci_data2[is.na(rci_data2$actualdate_endingdate),25] <- Sys.Date() 
  
  #I am using the current date for creating the column with elapsed months since the start
  #of the contract. I am assuming that the dataframe is up to date.
  
  rci_data2$elapsedmonths <- as.numeric(rci_data2$actualdate_endingdate - rci_data2$contract_start_date)
  rci_data2 <- rci_data2 %>% 
    mutate(elapsedmonths = round(elapsedmonths/30.417, digit=0))
  
  rci_data2$status_dummy <- rci_data2$contract_status
  
  rci_data2[rci_data2$status_dummy=="PREPAGADO",27] <- 0
  rci_data2[rci_data2$status_dummy=="ACTIVO",27] <- 1
  rci_data2$status_dummy <- as.factor(rci_data2$status_dummy)
  
  #It is better to work with numbers as these are probabilities.
  
  rci_split <- initial_split(rci_data2,prop=0.8)
  rci_training <- training(rci_split)
  rci_testing <- testing(rci_split)
  
  
  train_fit <- glm(status_dummy ~ elapsedmonths+age+total_income+
                     client_rate+ltv+bureau_score+vehicle_price+
                     financed_amount+object_quality, family = binomial, rci_training)
  
  summary(train_fit)
  
  rci_testing$predicted <- predict.glm(train_fit,rci_testing,type = "response")
  predicted <- predict.glm(train_fit,rci_testing,type = "response")
  
  chisq.test(rci_testing$status_dummy,rci_testing$predicted)

#P Value is significantly higher than usual the 0.05 level of confidence, so we cannot
#reject the null hypotheses. This means that our training model is a good fit
#for the testing set. 

#Finally lets build the confusion matrix.

library("caret")

confusionMatrix(rci_testing$status_dummy,rci_testing$predicted)

sensitivity(rci_testing$status_dummy,rci_testing$predicted) #True positive predictions.

misClassError(rci_testing$status_dummy,rci_testing$predicted) #Misclassification rate.

rci_testing$predictedrounded <- round(rci_testing$predicted)

rci_testing <- rci_testing %>% drop_na(status_dummy,predictedrounded)

write.csv(rci_testing,"rci_testing.csv")
write.csv(rci_training,"rci_training.csv")


