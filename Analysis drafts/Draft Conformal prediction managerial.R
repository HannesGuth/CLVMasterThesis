# 1. Get the variances
# 2. Get the intervals (acadmeic version) and collect the quantiles (managerial version)

# Set default settings
# splitweek = 130
ntraining = ceiling(length(unique(data1$Id))/2)
ntest = length(unique(data1$Id)) - ntraining
customers = unique(data1$Id)
CET_variance_table = data.table("Id" = customers,
                                "std" = 0)
PTS_variance_table = data.table("Id" = customers,
                                "std" = 0)

# First loop to get the averages over the differences (sds)
# for (i in 1:80){
#   print(i)
#   tryCatch( # sometimes, the model training returns an error, this result shall then be skipped
#     {
#       # Split the data set in training, validation and test
#       smp = sample(customers, ntraining, replace = FALSE)
#       train = gift1[Id %in% smp,]
#       test = gift1[!(Id %in% smp),]
#       
#       # Transform the split data into clvdata
#       trainCLV = clvdata(train,
#                          date.format="ymd", 
#                          time.unit = "week",
#                          estimation.split = splitWeek,
#                          name.id = "Id",
#                          name.date = "Date",
#                          name.price = "Price")
#       
#       testCLV = clvdata(test,
#                              date.format="ymd",
#                              time.unit = "week",
#                              estimation.split = splitWeek,
#                              name.id = "Id",
#                              name.date = "Date",
#                              name.price = "Price")
#       
#       # Create all models
#       trainModelpnbd = pnbd(clv.data = trainCLV)
#       testModelpnbd = pnbd(clv.data = testCLV)
# 
#       # Take the parameters from the training model for the other 2 models as well (only like this, it is possible
#       # to make predictions for calibration and test data using the train model)
#       testModelpnbd@prediction.params.model[1] = trainModelpnbd@prediction.params.model[1]
#       testModelpnbd@prediction.params.model[2] = trainModelpnbd@prediction.params.model[2]
#       testModelpnbd@prediction.params.model[3] = trainModelpnbd@prediction.params.model[3]
#       testModelpnbd@prediction.params.model[4] = trainModelpnbd@prediction.params.model[4]
#       
#       # Make predictions
#       results_pnbd_test = predict(testModelpnbd)
#       
#       # Create a table that later contains Id, Prediction, True values, error and quantiles for the calibration data
#       conformal_data_test = data.table("Id" = results_pnbd_test$Id,
#                                        "CET_prediction" = results_pnbd_test$CET,
#                                        "PTS_prediction" = results_pnbd_test$predicted.total.spending,
#                                        "CET_true" = results_pnbd_test$actual.x,
#                                        "PTS_true" = results_pnbd_test$actual.total.spending,
#                                        "CET_diff" = abs(results_pnbd_test$actual.x - results_pnbd_test$CET),
#                                        "PTS_diff" = abs(results_pnbd_test$actual.total.spending - results_pnbd_test$predicted.total.spending)
#       )
#       
#       # Collect the sds
#       CET_variance_table = merge(x = CET_variance_table, y = conformal_data_test[, c("Id", "CET_diff")], by = "Id", all.x = TRUE)
#       names(CET_variance_table)[length(CET_variance_table)] = paste("run", toString(length(names(CET_variance_table))))
#       PTS_variance_table = merge(x = PTS_variance_table, y = conformal_data_test[, c("Id", "PTS_diff")], by = "Id", all.x = TRUE)
#       names(PTS_variance_table)[length(PTS_variance_table)] = paste("run", toString(length(names(PTS_variance_table))))
#     },
#     # Catch errors
#     error = function(e){print(e)},
#     warning = function(w){print(w)}
#   )
# }

# Take the average over the differences of all runs
# for (i in 1:250){
#   CET_variance_table[i,2] = sqrt(var(as.vector(unlist(CET_variance_table[i,2:length(CET_variance_table)])), na.rm = TRUE))
#   PTS_variance_table[i,2] = sqrt(var(as.vector(unlist(PTS_variance_table[i,2:length(PTS_variance_table)])), na.rm = TRUE))
#   CET_variance_table[i,2] = mean(as.vector(unlist(CET_variance_table[i,2:length(CET_variance_table)])), na.rm = TRUE)
#   PTS_variance_table[i,2] = mean(as.vector(unlist(PTS_variance_table[i,2:length(PTS_variance_table)])), na.rm = TRUE)
# }

# # Table based on the differences from the loop
# cor_table = data.table("Id" = customers,
#                        "CET_pred" = results$CET,
#                        "CET_true" = results$actual.x,
#                        "CET_std" = CET_variance_table$CET_std,
#                        "PTS_pred" = results$predicted.total.spending,
#                        "PTS_true" = results$actual.total.spending,
#                        "PTS_std" = PTS_variance_table$PTS_std)
# 
# # Fit the linear models based on the 1. table
# CET_mod = lm(CET_std ~ CET_pred, data = cor_table)
# PTS_mod = lm(PTS_std ~ PTS_pred, data = cor_table)

clv.data1 <- clvdata(data1,  
                     date.format="ymd", 
                     time.unit = "week",
                     estimation.split = 130,
                     name.id = "Id",
                     name.date = "Date",
                     name.price = "Price")
est.data1 = pnbd(clv.data = clv.data1)
results.data1 = predict(est.data1, predict.spending = TRUE)

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

# ggplot(cor_table,aes(PTS_pred, PTS_std)) +
#   stat_summary(fun.data=mean_cl_normal) + 
#   geom_smooth(method='lm', formula= y~x)

# Set default settings 
alpha = 0.1
ntraining = ceiling(length(unique(data1$Id))/2)
ntest = length(unique(data1$Id)) - ntraining
customers = unique(data1$Id)
CET_LowerPI = data.table("Id" = customers,
                         "mean" = NA)
CET_UpperPI = data.table("Id" = customers,
                         "mean" = NA)
PTS_LowerPI = data.table("Id" = customers,
                         "mean" = NA)
PTS_UpperPI = data.table("Id" = customers,
                         "mean" = NA)
quantiles_PTS = list()
quantiles_CET = list()
q = ceiling(((ntraining + 1) * (1 - alpha)))/ntraining # see 9CP, equation 7
CET_validity_table = data.table("Id" = customers)
PTS_validity_table = data.table("Id" = customers)
parametertable = data.table("run" = seq(1:200),
                            "r" = 0,
                            "a" = 0,
                            "s" = 0,
                            "b" = 0,
                            "CET_accuracy" = 0,
                            "PTS_accuracy" = 0,
                            "CET_quantile" = 0,
                            "PTS_quantile" = 0)

# Train many models, use CP to predict intervals, average over the results
for (i in 1:1000){
  print(sum(is.na(CET_LowerPI$mean)))
  print(i)
  tryCatch( # sometimes, the model training returns an error, this result shall then be skipped
    {
      # Split the data set in training, validation and test
      smp = sample(customers, ntraining, replace = FALSE)
      train = data1[Id %in% smp,]
      test = data1[!(Id %in% smp),]
      
      # Transform the split data into clvdata
      trainCLV = clvdata(train,
                         date.format="ymd", 
                         time.unit = "week",
                         estimation.split = 130,
                         name.id = "Id",
                         name.date = "Date",
                         name.price = "Price")
      
      testCLV = clvdata(test,
                        date.format="ymd",
                        time.unit = "week",
                        estimation.split = 130,
                        name.id = "Id",
                        name.date = "Date",
                        name.price = "Price")
      
      # Create all models
      trainModelpnbd = pnbd(clv.data = trainCLV)
      testModelpnbd = pnbd(clv.data = testCLV)
      
      # Make predictions
      results_pnbd_train = predict(trainModelpnbd)
      results_pnbd_test = predict(testModelpnbd)
      
      # Create a table that later contains Id, Prediction, True values, error and quantiles for the calibration data
      conformal_data_train = data.table("Id" = results_pnbd_train$Id,
                                            "CET_prediction" = results_pnbd_train$CET,
                                            "PTS_prediction" = results_pnbd_train$predicted.total.spending,
                                            "CET_true" = results_pnbd_train$actual.x,
                                            "PTS_true" = results_pnbd_train$actual.total.spending
      )
      
      # Taking the estimated standard deviation without model fitting (1.1)
      # conformal_data_calibrate = merge(x = conformal_data_calibrate, y = CET_variance_table[, c("Id", "CET_std")], all.x = TRUE)
      # conformal_data_calibrate = merge(x = conformal_data_calibrate, y = PTS_variance_table[, c("Id", "PTS_std")], all.x = TRUE)
      
      # Predict the sds of the calibration customers
      conformal_data_train$CET_std = predict(CET_mod, data.frame(CET_pred = conformal_data_train$CET_prediction))
      conformal_data_train$PTS_std = predict(PTS_mod, data.frame(PTS_pred = conformal_data_train$PTS_prediction))
      # Get the error quantiles in this run
      
      # Taking the estimated standard deviation without model fitting (1.1)
      # conformal_data_calibrate$CET_error = abs(conformal_data_calibrate$CET_prediction - conformal_data_calibrate$CET_true)/conformal_data_calibrate$CET_std
      # conformal_data_calibrate$PTS_error = abs(conformal_data_calibrate$PTS_prediction - conformal_data_calibrate$PTS_true)/conformal_data_calibrate$PTS_std
      
      # Get the residuals
      conformal_data_train$CET_error = abs(conformal_data_train$CET_prediction - conformal_data_train$CET_true)/predict(CET_mod, data.frame(CET_pred = conformal_data_train$CET_prediction))
      conformal_data_train$PTS_error = abs(conformal_data_train$PTS_prediction - conformal_data_train$PTS_true)/predict(PTS_mod, data.frame(PTS_pred = conformal_data_train$PTS_prediction))
      
      # Get the quantiles
      quantile_CET = quantile(conformal_data_train$CET_error, q)
      quantile_PTS = quantile(conformal_data_train$PTS_error, q)
      if (quantile_PTS == 1 | quantile_CET == 1) next
      quantiles_CET = append(quantiles_CET, quantile_CET)
      quantiles_PTS = append(quantiles_PTS, quantile_PTS)
      print(paste("quantile_CET:", quantile_CET))
      print(paste("quantile_PTS:", quantile_PTS))
      
      # Predict the intervals in this run for the calibration data (actually not needed)
      conformal_data_train$CET_Lower90 = conformal_data_train$CET_prediction - (conformal_data_train$CET_std * quantile_CET)
      conformal_data_train$CET_Upper90 = conformal_data_train$CET_prediction + (conformal_data_train$CET_std * quantile_CET)
      conformal_data_train$PTS_Lower90 = conformal_data_train$PTS_prediction - (conformal_data_train$PTS_std * quantile_PTS)
      conformal_data_train$PTS_Upper90 = conformal_data_train$PTS_prediction + (conformal_data_train$PTS_std * quantile_PTS)
      
      # Create a table for the test data that contains the PIs for each customer in this run and if the true value is covered
      conformal_data_test = data.table("Id" = results_pnbd_test$Id,
                                      "CET_prediction" = results_pnbd_test$CET,
                                      "PTS_prediction" = results_pnbd_test$predicted.total.spending
      )
      
      conformal_data_test = merge(x = conformal_data_test, y = results_pnbd_test[,c("Id", "actual.x", "actual.total.spending")], by = "Id")
      
      # Taking the estimated standard deviation without model fitting (1.1)
      # conformaldata_test = merge(x = conformaldata_test, y = CET_variance_table[, c("Id", "CET_std")])
      # conformaldata_test = merge(x = conformaldata_test, y = PTS_variance_table[, c("Id", "PTS_std")])
      
      # Get the sds of the test customers
      conformal_data_test$CET_std = predict(CET_mod, data.frame(CET_pred = conformal_data_test$CET_prediction))
      conformal_data_test$PTS_std = predict(PTS_mod, data.frame(PTS_pred = conformal_data_test$PTS_prediction))
      
      conformal_data_test$CET_Lower90 = conformal_data_test$CET_prediction - (conformal_data_test$CET_std * quantile_CET)
      conformal_data_test$CET_Upper90 = conformal_data_test$CET_prediction + (conformal_data_test$CET_std * quantile_CET)
      conformal_data_test$PTS_Lower90 = conformal_data_test$PTS_prediction - (conformal_data_test$PTS_std * quantile_PTS)
      conformal_data_test$PTS_Upper90 = conformal_data_test$PTS_prediction + (conformal_data_test$PTS_std * quantile_PTS)
      
      conformal_data_test$CET_covered = ifelse(conformal_data_test$CET_Lower90 < conformal_data_test$actual.x & conformal_data_test$CET_Upper90 > conformal_data_test$actual.x, 1, 0)
      conformal_data_test$PTS_covered = ifelse(conformal_data_test$PTS_Lower90 < conformal_data_test$actual.total.spending & conformal_data_test$PTS_Upper90 > conformal_data_test$actual.total.spending, 1, 0)
      
      # Fill the validity table for this column/run
      CET_validity_table = merge(x = CET_validity_table, y = conformal_data_test[, c("Id", "CET_covered")], by = "Id", all.x = TRUE)
      names(CET_validity_table)[length(names(CET_validity_table))] = paste("run", toString(length(names(CET_validity_table))))
      PTS_validity_table = merge(x = PTS_validity_table, y = conformal_data_test[, c("Id", "PTS_covered")], by = "Id", all.x = TRUE)
      names(PTS_validity_table)[length(names(PTS_validity_table))] = paste("run", toString(length(names(PTS_validity_table))))
      
      # Transfer the results to LowerPI and UpperPI
      CET_LowerPI = merge(x = CET_LowerPI, y = conformal_data_test[,c("Id", "CET_Lower90")], by = "Id", all.x = TRUE)
      CET_UpperPI = merge(x = CET_UpperPI, y = conformal_data_test[,c("Id", "CET_Upper90")], by = "Id", all.x = TRUE)
      PTS_LowerPI = merge(x = PTS_LowerPI, y = conformal_data_test[,c("Id", "PTS_Lower90")], by = "Id", all.x = TRUE)
      PTS_UpperPI = merge(x = PTS_UpperPI, y = conformal_data_test[,c("Id", "PTS_Upper90")], by = "Id", all.x = TRUE)
      names(CET_LowerPI)[length(names(CET_LowerPI))] = paste(toString(length(names(CET_LowerPI))))
      names(CET_UpperPI)[length(names(CET_UpperPI))] = paste(toString(length(names(CET_UpperPI))))
      names(PTS_LowerPI)[length(names(PTS_LowerPI))] = paste(toString(length(names(PTS_LowerPI))))
      names(PTS_UpperPI)[length(names(PTS_UpperPI))] = paste(toString(length(names(PTS_UpperPI))))
      CET_LowerPI$mean = rowMeans(CET_LowerPI[,!c("Id","mean")], na.rm = TRUE)
      CET_UpperPI$mean = rowMeans(CET_UpperPI[,!c("Id","mean")], na.rm = TRUE)
      PTS_LowerPI$mean = rowMeans(PTS_LowerPI[,!c("Id","mean")], na.rm = TRUE)
      PTS_UpperPI$mean = rowMeans(PTS_UpperPI[,!c("Id","mean")], na.rm = TRUE)
      
      # Not directly part of Conformal prediction
      parametertable[i, 2] = trainModelpnbd@prediction.params.model[1]
      parametertable[i, 3] = trainModelpnbd@prediction.params.model[2]
      parametertable[i, 4] = trainModelpnbd@prediction.params.model[3]
      parametertable[i, 5] = trainModelpnbd@prediction.params.model[4]
      parametertable[i, 6] = mean(conformal_data_test$CET_covered)
      parametertable[i, 7] = mean(conformal_data_test$PTS_covered)
      parametertable[i, 8] = as.numeric(quantile_CET)
      parametertable[i, 9] = as.numeric(quantile_PTS)
    },
    # Catch errors
    error = function(e){print(e)},
    warning = function(w){print(w)}
  )
}

# Create the resulting table for comparison with other methods
intervals_CP = data.table("Id" = results.data1$Id,
                            "CET_lower" = max(0, CET_LowerPI$mean),
                            "CET_upper" = CET_UpperPI$mean,
                            "CET_true" = results.data1$actual.x,
                            "CET_prediction" = results.data1$CET,
                            "CET_covered" = CET_LowerPI$mean < results.data1$actual.x & CET_UpperPI$mean > results.data1$actual.x,
                            "PTS_lower" = max(0, PTS_LowerPI$mean),
                            "PTS_upper" = PTS_UpperPI$mean,
                            "PTS_true" = results.data1$actual.total.spending,
                            "PTS_prediction" = results.data1$predicted.total.spending,
                            "PTS_covered" = PTS_LowerPI$mean < results.data1$actual.total.spending & PTS_UpperPI$mean > results.data1$actual.total.spending
)

###################
# From here analysis of the performance, not directly part of Conformal prediction

# Performance
sum(intervals_CP$CET_covered)/nrow(intervals_CP)
sum(intervals_CP$PTS_covered)/nrow(intervals_CP)

# Measure validity
mean(as.vector(unlist(CET_validity_table[,2:length(CET_validity_table)])), na.rm = TRUE)
mean(as.vector(unlist(PTS_validity_table[,2:length(PTS_validity_table)])), na.rm = TRUE)

CET_validities = list()
PTS_validities = list()
for (i in 2:length(CET_validity_table)){
  CET_validities = append(CET_validities, mean(unlist(CET_validity_table[,..i]), na.rm = TRUE))
  PTS_validities = append(PTS_validities, mean(unlist(PTS_validity_table[,..i]), na.rm = TRUE))
  print(paste("i", i, ": ", mean(unlist(CET_validity_table[,..i]), na.rm = TRUE)))
  print(paste("i", i, ": ", mean(unlist(PTS_validity_table[,..i]), na.rm = TRUE)))
}

CET_validities = as.vector(unlist(CET_validities))
hist(CET_validities)
plot(density(CET_validities))

PTS_validities = as.vector(unlist(PTS_validities))
hist(PTS_validities)
plot(density(PTS_validities))

###################
# Managerial version

val = data.table("run" = seq(1,length(CET_validities),1),
                 "CET_validities" = CET_validities,
                 "PTS_validities" = PTS_validities,
                 "CET_average" = 0,
                 "PTS_average" = 0)

for (i in 1:nrow(val)){
  val[i,4] = mean(as.vector(unlist(val[1:i,2])))
  val[i,5] = mean(as.vector(unlist(val[1:i,3])))
}

print(ggplot(val) +
  geom_line(aes(x = run, y = CET_average), color = "red") +
  geom_line(aes(x = run, y = PTS_average), color = "green")
)
  #geom_point(aes(y = PTS_validities))





clv.data2 <- clvdata(data2,  
                     date.format="ymd", 
                     time.unit = "week",
                     estimation.split = 130,
                     name.id = "Id",
                     name.date = "Date",
                     name.price = "Price")
est.data2 = pnbd(clv.data = clv.data2)
results.data2 = predict(est.data2, predict.spending = TRUE)

CET_quantile = mean(as.vector(unlist(quantiles_CET)))
PTS_quantile = mean(as.vector(unlist(quantiles_PTS)))

CET_std = predict(CET_mod, data.frame(CET_pred = results.data2$CET))
PTS_std = predict(PTS_mod, data.frame(PTS_pred = results.data2$predicted.total.spending))

intervals_CP_m = data.table("Id" = results.data2$Id,
                          "CET_lower" = results.data2$CET - (CET_std * CET_quantile),
                          "CET_upper" = results.data2$CET + (CET_std * CET_quantile),
                          "CET_true" = results.data2$actual.x,
                          "CET_prediction" = results.data2$CET,
                          "CET_covered" = 0,
                          "PTS_lower" = results.data2$predicted.total.spending - (PTS_std * CET_quantile),
                          "PTS_upper" = results.data2$predicted.total.spending + (PTS_std * CET_quantile),
                          "PTS_true" = results.data2$actual.total.spending,
                          "PTS_prediction" = results.data2$predicted.total.spending,
                          "PTS_covered" = 0
)

intervals_CP_m$CET_covered = intervals_CP_m$CET_lower <= intervals_CP_m$CET_true & intervals_CP_m$CET_upper >= intervals_CP_m$CET_true
intervals_CP_m$PTS_covered = intervals_CP_m$PTS_lower <= intervals_CP_m$PTS_true & intervals_CP_m$PTS_upper >= intervals_CP_m$PTS_true
mean(intervals_CP_m$CET_covered)
mean(intervals_CP_m$PTS_covered)

