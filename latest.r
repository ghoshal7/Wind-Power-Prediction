##### Data Preparation
setwd('D:/USA/Isen MS/619 - Predictive Analysis/project')
library(readxl)
data_15 <- read_excel('TrainingData_2015.xlsx')
data_15 <- as.data.frame(data_15)

## Add Air Density & Turbulance
P = 101325
R = 287
library(lubridate)
data_15['Air Density'] =  P/(R*(273+data_15$`Environment Temperature`))
data_15['Turbulence Intensity'] = data_15[,3]/data_15[,2]
data_15['Month'] = month(as.POSIXlt(data_15[,1]))
data_15['hour'] = hour(as.POSIXlt(data_15[,1]))

# Check for NA
counter = 0
for (i in 1:5){
  if (is.na(data_15[,i]) == 1){
    print ('There is missing values')
  } else {
    counter = counter+1
  }
}
if (counter == 5){
  print('Data is good')
}

# Sorting the data
data_15 = data_15[order(data_15[,2]),]

#### EDA
summary(data_15)
head(data_15)

pairs(data_15[,-1])

morng = subset(data_15, (data_15['hour'] == 6))
morng1 = subset(data_15, (data_15['hour'] == 9))
noon = subset(data_15, (data_15['hour'] == 12))
noon1 = subset(data_15, (data_15['hour'] == 15))
evng = subset(data_15, (data_15['hour'] == 18))
evng1 = subset(data_15, (data_15['hour'] == 21))
night = subset(data_15, (data_15['hour'] == 24))
night1 = subset(data_15, (data_15['hour'] == 3))

plot(morng[,2], morng[,6], col = 'red', xlab = 'Wind Speed', ylab = 'Power Output',
     main = 'Speed Vs. Power | Hour')
points(morng1[,2], morng1[,6], col = 'blue')
points(noon[,2], noon[,6], col = 'yellow')
points(noon1[,2], noon1[,6], col = 'green')
points(evng[,2], evng[,6], col = 'orange')
points(evng1[,2], evng1[,6], col = 'magenta')
points(night[,2], night[,6], col = 'black')
points(night1[,2], night1[,6], col = 'brown')
#legend('topright', c('Early Morning', 'Morning', 'Noon', 'Afternoon', 'Evening', 'Night', 'Midnight', 'Dawn'), 
       text.col = 'red', 'blue', 'yellow', 'green', 'orange', 'magenta','black', 'brown')

feb = subset(data_15, (data_15['Month'] == 2))
may = subset(data_15, (data_15['Month'] == 5))
aug = subset(data_15, (data_15['Month'] == 8))
nov = subset(data_15, (data_15['Month'] == 11))

plot(feb[,2], feb[,6], col = 'red', xlab = 'Wind Speed', ylab = 'Power Output',
     main = 'Speed Vs. Power | Month')
points(may[,2], may[,6], col = 'blue')
points(aug[,2], aug[,6], col = 'green')
points(nov[,2], nov[,6], col = 'yellow')



## SVM + XGBoost
library('xgboost')
library(e1071)

dta_15 = data_15[,-1]

RMSE = function(y.fit, y){
  sqrt(mean((y.fit - y)^2))
}

k_folds <- 10
folds_i <- sample(rep(1:k_folds, length.out = nrow(data_15)))
cv_tmp <- matrix(NA, k_folds, 3)
for (k in 1:k_folds) {
    indx = which(folds_i == k)
    train = dta_15[-indx, ]
    test = dta_15[indx, ]
    
    xgbFit = xgboost(data = as.matrix(train[,-5]), nfold = 5, label = train[,5],nrounds = 500,
                     max_depth = 6, objective = "reg:linear", eval_metric = "rmse", eta = 0.05, verbose = FALSE)
    xgb_out = predict(xgbFit, newdata = as.matrix(test[,-5]))
    print('SVM Starts...')
    svm.model <- svm(train$Power_Avg ~ .-train$Power_Avg, data = train, cost = 10, gamma = 1,type='eps')
    svm_out = predict(svm.model,test[,-5])
    print('SVM Finishes...')
    pred = (0.4*svm_out)+(0.6*xgb_out)
    
    cv_tmp[k,1] = RMSE(test[,5], xgb_out)
    cv_tmp[k,2] = RMSE(test[,5], svm_out)
    cv_tmp[k,3] = RMSE(test[,5], pred)
    
    print(cv_tmp[k,3])
  }
}
cv <- mean(cv_tmp)

sprintf('cross validation error = %s', cv) #0.0273921261675339
