# 1. Get the variances
# 2. Get the intervals (academic version) and collect the quantiles (managerial version)

# Set default settings

ntraining = ceiling(length(unique(data1$Id))*0.8)
ntest = length(unique(data1$Id)) - ntraining
customers = unique(data1$Id)

clv.data1 <- clvdata(data1,
                     date.format="ymd",
                     time.unit = "week",
                     estimation.split = splitweek1,
                     name.id = "Id",
                     name.date = "Date",
                     name.price = "Price")
est.data1 = pnbd(clv.data = clv.data1)

if (whole_period1){
  results.data1 = predict(est.data1, predict.spending = TRUE)
}else{
  results.data1 = predict(est.data1, predict.spending = TRUE, prediction.end = end1)
}

# Table based on differences from a single fit
cor_table = data.table("Id" = customers,
                       "CET_pred" = results.data1$CET,
                       "CET_true" = results.data1$actual.x,
                       "CET_std" = abs(results.data1$actual.x - results.data1$CET),
                       "PTS_pred" = results.data1$predicted.total.spending,
                       "PTS_true" = results.data1$actual.total.spending,
                       "PTS_std" = abs(results.data1$actual.total.spending - results.data1$predicted.total.spending))

# Fit the linear models based on the 2. table
CET_mod = lm(CET_std ~ CET_pred, data = cor_table)
PTS_mod = lm(PTS_std ~ PTS_pred, data = cor_table)

# Set default settings 
alpha = 0.1
n = 20
q = ceiling(((ntraining + 1) * (1 - alpha)))/ntraining # see 9CP, equation 7

quantiles_CET = list()
quantiles_PTS = list()
#########

for (i in 1:n){
  tryCatch(
    {      
      # Split the data set in training, validation and test
      smp = sample(customers, ntraining, replace = FALSE)
      train = data1[Id %in% smp,]
      test = data1[!(Id %in% smp),]
      print(i)
      
      # Transform the split data into clvdata
      trainCLV = clvdata(train,
                         date.format="ymd", 
                         time.unit = "week",
                         estimation.split = splitweek1,
                         name.id = "Id",
                         name.date = "Date",
                         name.price = "Price")
      
      testCLV = clvdata(test,
                        date.format="ymd",
                        time.unit = "week",
                        estimation.split = splitweek1,
                        name.id = "Id",
                        name.date = "Date",
                        name.price = "Price")
      
      # Create all models
      trainModelpnbd = pnbd(clv.data = trainCLV)
      testModelpnbd = pnbd(clv.data = testCLV)
    
      # Make predictions
      if (whole_period1){
        results_pnbd_train = predict(trainModelpnbd)
        results_pnbd_test = predict(testModelpnbd)
      }else{
        results_pnbd_train = predict(trainModelpnbd, prediction.end = end1)
        results_pnbd_test = predict(testModelpnbd, prediction.end = end1)
      }
      
      
      # Create a table that later contains Id, Prediction, True values, error and quantiles for the calibration data
      conformal_data_train = data.table("Id" = results_pnbd_train$Id,
                                        "CET_prediction" = results_pnbd_train$CET,
                                        "PTS_prediction" = results_pnbd_train$predicted.total.spending,
                                        "CET_true" = results_pnbd_train$actual.x,
                                        "PTS_true" = results_pnbd_train$actual.total.spending
      )
      
      conformal_data_train$CET_std = predict(CET_mod, data.frame(CET_pred = conformal_data_train$CET_prediction))
      conformal_data_train$PTS_std = predict(PTS_mod, data.frame(PTS_pred = conformal_data_train$PTS_prediction))
      
      # Get the residuals
      conformal_data_train$CET_error = abs(conformal_data_train$CET_prediction - conformal_data_train$CET_true)/conformal_data_train$CET_std
      conformal_data_train$PTS_error = abs(conformal_data_train$PTS_prediction - conformal_data_train$PTS_true)/conformal_data_train$PTS_std
      
      quantile_CET = quantile(conformal_data_train$CET_error, q)
      quantile_PTS = quantile(conformal_data_train$PTS_error, q)
      quantiles_CET = append(quantiles_CET, quantile_CET)
      quantiles_PTS = append(quantiles_PTS, quantile_PTS)
      
      # Create a table for the test data that contains the PIs for each customer in this run and if the true value is covered
      conformal_data_test = data.table("Id" = results_pnbd_test$Id,
                                       "CET_prediction" = results_pnbd_test$CET,
                                       "PTS_prediction" = results_pnbd_test$predicted.total.spending
      )
      
      conformal_data_test = merge(x = conformal_data_test, y = results_pnbd_test[,c("Id", "actual.x", "actual.total.spending")], by = "Id")
      
      # Get the sds of the test customers
      conformal_data_test$CET_std = predict(CET_mod, data.frame(CET_pred = conformal_data_test$CET_prediction))
      conformal_data_test$PTS_std = predict(PTS_mod, data.frame(PTS_pred = conformal_data_test$PTS_prediction))
      
      # Calculate the boundaries
      conformal_data_test$CET_Lower90 = conformal_data_test$CET_prediction - (conformal_data_test$CET_std * quantile_CET)
      conformal_data_test$CET_Upper90 = conformal_data_test$CET_prediction + (conformal_data_test$CET_std * quantile_CET)
      conformal_data_test$PTS_Lower90 = conformal_data_test$PTS_prediction - (conformal_data_test$PTS_std * quantile_PTS)
      conformal_data_test$PTS_Upper90 = conformal_data_test$PTS_prediction + (conformal_data_test$PTS_std * quantile_PTS)
      
      # Calculate the coverage
      conformal_data_test$CET_covered = ifelse(conformal_data_test$CET_Lower90 < conformal_data_test$actual.x & conformal_data_test$CET_Upper90 > conformal_data_test$actual.x, 1, 0)
      conformal_data_test$PTS_covered = ifelse(conformal_data_test$PTS_Lower90 < conformal_data_test$actual.total.spending & conformal_data_test$PTS_Upper90 > conformal_data_test$actual.total.spending, 1, 0)
    },
    error = function(e){print("Error")}, #intervals_CP_m_rep[,c(6,11)]= NA
    warning = function(w){print("Error")}
  )
}
    
###################
# Managerial version

quantile_CET = mean(unlist(quantiles_CET))
quantile_PTS = mean(unlist(quantiles_PTS))
quantile_CET_rep = quantile_CET
quantile_PTS_rep = quantile_PTS

clv.data2 <- clvdata(data2,
                     date.format="ymd", 
                     time.unit = "week",
                     estimation.split = splitweek2,
                     name.id = "Id",
                     name.date = "Date",
                     name.price = "Price")
est.data2 = pnbd(clv.data = clv.data2)

if (whole_period2){
 results.data2 = predict(est.data2, predict.spending = TRUE)
}else{
  results.data2 = predict(est.data2, predict.spending = TRUE, prediction.end = end2)
}


CET_std = predict(CET_mod, data.frame(CET_pred = results.data2$CET))
PTS_std = predict(PTS_mod, data.frame(PTS_pred = results.data2$predicted.total.spending))

intervals_CR_m = data.table("Id" = results.data2$Id,
                          "CET_lower" = ifelse((results.data2$CET - (CET_std * quantile_CET)) < 0, 0, results.data2$CET - (CET_std * quantile_CET)),
                          "CET_upper" = results.data2$CET + (CET_std * quantile_CET),
                          "CET_true" = results.data2$actual.x,
                          "CET_prediction" = results.data2$CET,
                          "CET_covered" = 0,
                          "PTS_lower" = ifelse((results.data2$predicted.total.spending - (PTS_std * quantile_PTS)) < 0, 0, results.data2$predicted.total.spending - (PTS_std * quantile_PTS)),
                          "PTS_upper" = results.data2$predicted.total.spending + (PTS_std * quantile_PTS),
                          "PTS_true" = results.data2$actual.total.spending,
                          "PTS_prediction" = results.data2$predicted.total.spending,
                          "PTS_covered" = 0
)

intervals_CR_m$CET_covered = intervals_CR_m$CET_lower <= intervals_CR_m$CET_true & intervals_CR_m$CET_upper >= intervals_CR_m$CET_true
intervals_CR_m$PTS_covered = intervals_CR_m$PTS_lower <= intervals_CR_m$PTS_true & intervals_CR_m$PTS_upper >= intervals_CR_m$PTS_true
mean(intervals_CR_m$CET_covered)
mean(intervals_CR_m$PTS_covered)

