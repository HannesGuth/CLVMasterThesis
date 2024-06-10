# 1. Get the variances
# 2. Get the intervals (acadmeic version) and collect the quantiles (managerial version)

# Set default settings
ntraining = 125
ncalibration = 90
ntest = 250 - ntraining
customers = unique(apparelTrans$Id)
CET_variance_table = data.table("Id" = customers,
                                "std" = 0)
PTS_variance_table = data.table("Id" = customers,
                                "std" = 0)

# First loop to get the averages over the differences (sds)
for (i in 1:80){
  print(i)
  tryCatch( # sometimes, the model training returns an error, this result shall then be skipped
    {
      # Split the data set in training, validation and test
      smp = sample(customers, ntraining, replace = FALSE)
      train = apparelTrans[Id %in% smp,]
      test = apparelTrans[!(Id %in% smp),]
      
      # Transform the split data into clvdata
      trainCLV = clvdata(train,
                         date.format="ymd", 
                         time.unit = "week",
                         estimation.split = splitWeek,
                         name.id = "Id",
                         name.date = "Date",
                         name.price = "Price")
      
      testCLV = clvdata(test,
                             date.format="ymd",
                             time.unit = "week",
                             estimation.split = splitWeek,
                             name.id = "Id",
                             name.date = "Date",
                             name.price = "Price")
      
      # Create all models
      trainModelpnbd = pnbd(clv.data = trainCLV)
      testModelpnbd = pnbd(clv.data = testCLV)

      # Take the parameters from the training model for the other 2 models as well (only like this, it is possible
      # to make predictions for calibration and test data using the train model)
      testModelpnbd@prediction.params.model[1] = trainModelpnbd@prediction.params.model[1]
      testModelpnbd@prediction.params.model[2] = trainModelpnbd@prediction.params.model[2]
      testModelpnbd@prediction.params.model[3] = trainModelpnbd@prediction.params.model[3]
      testModelpnbd@prediction.params.model[4] = trainModelpnbd@prediction.params.model[4]
      
      # Make predictions
      results_pnbd_test = predict(testModelpnbd)
      
      # Create a table that later contains Id, Prediction, True values, error and quantiles for the calibration data
      conformal_data_test = data.table("Id" = results_pnbd_test$Id,
                                       "CET_prediction" = results_pnbd_test$CET,
                                       "PTS_prediction" = results_pnbd_test$predicted.total.spending,
                                       "CET_true" = results_pnbd_test$actual.x,
                                       "PTS_true" = results_pnbd_test$actual.total.spending,
                                       "CET_diff" = abs(results_pnbd_test$actual.x - results_pnbd_test$CET),
                                       "PTS_diff" = abs(results_pnbd_test$actual.total.spending - results_pnbd_test$predicted.total.spending)
      )
      
      # Collect the sds
      CET_variance_table = merge(x = CET_variance_table, y = conformal_data_test[, c("Id", "CET_diff")], by = "Id", all.x = TRUE)
      names(CET_variance_table)[length(CET_variance_table)] = paste("run", toString(length(names(CET_variance_table))))
      PTS_variance_table = merge(x = PTS_variance_table, y = conformal_data_test[, c("Id", "PTS_diff")], by = "Id", all.x = TRUE)
      names(PTS_variance_table)[length(PTS_variance_table)] = paste("run", toString(length(names(PTS_variance_table))))
    },
    # Catch errors
    error = function(e){print(e)},
    warning = function(w){print(w)}
  )
}

# Take the average over the differences of all runs
for (i in 1:250){
  # CET_variance_table[i,2] = sqrt(var(as.vector(unlist(CET_variance_table[i,2:length(CET_variance_table)])), na.rm = TRUE))
  # PTS_variance_table[i,2] = sqrt(var(as.vector(unlist(PTS_variance_table[i,2:length(PTS_variance_table)])), na.rm = TRUE))
  CET_variance_table[i,2] = mean(as.vector(unlist(CET_variance_table[i,2:length(CET_variance_table)])), na.rm = TRUE)
  PTS_variance_table[i,2] = mean(as.vector(unlist(PTS_variance_table[i,2:length(PTS_variance_table)])), na.rm = TRUE)
}

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

# Table based on differences from a single fit
cor_table = data.table("Id" = customers,
                       "CET_pred" = results$CET,
                       "CET_true" = results$actual.x,
                       "CET_std" = abs(results$actual.x - results$CET),
                       "PTS_pred" = results$predicted.total.spending,
                       "PTS_true" = results$actual.total.spending,
                       "PTS_std" = abs(results$actual.total.spending - results$predicted.total.spending))

# Fit the linear models based on the 2. table
CET_mod = lm(CET_std ~ CET_pred, data = cor_table)
PTS_mod = lm(PTS_std ~ PTS_pred, data = cor_table)

# Set default settings 
alpha = 0.1
ntraining = 50
ncalibration = 90
ntest = 250 - ntraining - ncalibration
customers = unique(apparelTrans$Id)
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
q = ceiling(((ncalibration + 1) * (1 - alpha)))/ncalibration # see 9CP, equation 7
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
for (i in 1:80){
  print(sum(is.na(CET_LowerPI$mean)))
  print(i)
  tryCatch( # sometimes, the model training returns an error, this result shall then be skipped
    {
      # Split the data set in training, validation and test
      smp = sample(customers, ntraining, replace = FALSE)
      test = apparelTrans[Id %in% smp,]
      trca = apparelTrans[!(Id %in% smp),]
      ids = sample(unique(trca$Id), ntest)
      trca$class = ifelse(trca$Id %in% ids, "train", "calibrate")
      train = trca[class == "train",]
      calibrate = trca[class == "calibrate",]
      
      # Transform the split data into clvdata
      trainCLV = clvdata(train,
                         date.format="ymd", 
                         time.unit = "week",
                         estimation.split = splitWeek,
                         name.id = "Id",
                         name.date = "Date",
                         name.price = "Price")
      
      calibrateCLV = clvdata(calibrate,
                             date.format="ymd",
                             time.unit = "week",
                             estimation.split = splitWeek,
                             name.id = "Id",
                             name.date = "Date",
                             name.price = "Price")
      
      testCLV = clvdata(test,
                        date.format="ymd",
                        time.unit = "week",
                        estimation.split = splitWeek,
                        name.id = "Id",
                        name.date = "Date",
                        name.price = "Price")
      
      # Create all models
      trainModelpnbd = pnbd(clv.data = trainCLV)
      calibrateModelpnbd = pnbd(clv.data = calibrateCLV)
      testModelpnbd = pnbd(clv.data = testCLV)
      
      # Take the parameters from the training model for the other 2 models as well (only like this, it is possible
      # to make predictions for calibration and test data using the train model)
      calibrateModelpnbd@prediction.params.model[1] = trainModelpnbd@prediction.params.model[1]
      calibrateModelpnbd@prediction.params.model[2] = trainModelpnbd@prediction.params.model[2]
      calibrateModelpnbd@prediction.params.model[3] = trainModelpnbd@prediction.params.model[3]
      calibrateModelpnbd@prediction.params.model[4] = trainModelpnbd@prediction.params.model[4]
      
      testModelpnbd@prediction.params.model[1] = trainModelpnbd@prediction.params.model[1]
      testModelpnbd@prediction.params.model[2] = trainModelpnbd@prediction.params.model[2]
      testModelpnbd@prediction.params.model[3] = trainModelpnbd@prediction.params.model[3]
      testModelpnbd@prediction.params.model[4] = trainModelpnbd@prediction.params.model[4]
      
      # Make predictions
      results_pnbd_calibrate = predict(calibrateModelpnbd)
      # results_gg_calibrate = predict(calibrateModelgg)
      results_pnbd_test = predict(testModelpnbd)
      # results_gg_test = predict(testModelgg)
      
      
      # Create a table that later contains Id, Prediction, True values, error and quantiles for the calibration data
      conformal_data_calibrate = data.table("Id" = results_pnbd_calibrate$Id,
                                            "CET_prediction" = results_pnbd_calibrate$CET,
                                            "PTS_prediction" = results_pnbd_calibrate$predicted.total.spending,
                                            "CET_true" = results_pnbd_calibrate$actual.x,
                                            "PTS_true" = results_pnbd_calibrate$actual.total.spending
      )
      
      # Taking the estimated standard deviation without model fitting (1.1)
      # conformal_data_calibrate = merge(x = conformal_data_calibrate, y = CET_variance_table[, c("Id", "CET_std")], all.x = TRUE)
      # conformal_data_calibrate = merge(x = conformal_data_calibrate, y = PTS_variance_table[, c("Id", "PTS_std")], all.x = TRUE)
      
      # Predict the sds of the calibration customers
      conformal_data_calibrate$CET_std = predict(CET_mod, data.frame(CET_pred = conformal_data_calibrate$CET_prediction))
      conformal_data_calibrate$PTS_std = predict(PTS_mod, data.frame(PTS_pred = conformal_data_calibrate$PTS_prediction))
      # Get the error quantiles in this run
      
      # Taking the estimated standard deviation without model fitting (1.1)
      # conformal_data_calibrate$CET_error = abs(conformal_data_calibrate$CET_prediction - conformal_data_calibrate$CET_true)/conformal_data_calibrate$CET_std
      # conformal_data_calibrate$PTS_error = abs(conformal_data_calibrate$PTS_prediction - conformal_data_calibrate$PTS_true)/conformal_data_calibrate$PTS_std
      
      # Get the residuals
      conformal_data_calibrate$CET_error = abs(conformal_data_calibrate$CET_prediction - conformal_data_calibrate$CET_true)/predict(CET_mod, data.frame(CET_pred = conformal_data_calibrate$CET_prediction))
      conformal_data_calibrate$PTS_error = abs(conformal_data_calibrate$PTS_prediction - conformal_data_calibrate$PTS_true)/predict(PTS_mod, data.frame(PTS_pred = conformal_data_calibrate$PTS_prediction))
      
      # Get the quantiles
      quantile_CET = quantile(conformal_data_calibrate$CET_error, q)
      quantile_PTS = quantile(conformal_data_calibrate$PTS_error, q)
      if (quantile_PTS == 1 | quantile_CET == 1) next
      quantiles_CET = append(quantiles_CET, quantile_CET)
      quantiles_PTS = append(quantiles_PTS, quantile_PTS)
      print(paste("quantile_CET:", quantile_CET))
      print(paste("quantile_PTS:", quantile_PTS))
      
      # Predict the intervals in this run for the calibration data (actually not needed)
      conformal_data_calibrate$CET_Lower90 = max(0, conformal_data_calibrate$CET_prediction - (conformal_data_calibrate$CET_std * quantile_CET))
      conformal_data_calibrate$CET_Upper90 = conformal_data_calibrate$CET_prediction + (conformal_data_calibrate$CET_std * quantile_CET)
      conformal_data_calibrate$PTS_Lower90 = max(0, conformal_data_calibrate$PTS_prediction - (conformal_data_calibrate$PTS_std * quantile_PTS))
      conformal_data_calibrate$PTS_Upper90 = conformal_data_calibrate$PTS_prediction + (conformal_data_calibrate$PTS_std * quantile_PTS)
      
      # Create a table for the test data that contains the PIs for each customer in this run and if the true value is covered
      conformaldata_test = data.table("Id" = results_pnbd_test$Id,
                                      "CET_prediction" = results_pnbd_test$CET,
                                      "PTS_prediction" = results_pnbd_test$predicted.total.spending
      )
      
      conformaldata_test = merge(x = conformaldata_test, y = results[,c("Id", "actual.x", "actual.total.spending")], by = "Id")
      
      # Taking the estimated standard deviation without model fitting (1.1)
      # conformaldata_test = merge(x = conformaldata_test, y = CET_variance_table[, c("Id", "CET_std")])
      # conformaldata_test = merge(x = conformaldata_test, y = PTS_variance_table[, c("Id", "PTS_std")])
      
      # Get the sds of the test customers
      conformaldata_test$CET_std = predict(CET_mod, data.frame(CET_pred = conformaldata_test$CET_prediction))
      conformaldata_test$PTS_std = predict(PTS_mod, data.frame(PTS_pred = conformaldata_test$PTS_prediction))
      
      conformaldata_test$CET_Lower90 = max(0, conformaldata_test$CET_prediction - (conformaldata_test$CET_std * quantile_CET))
      conformaldata_test$CET_Upper90 = conformaldata_test$CET_prediction + (conformaldata_test$CET_std * quantile_CET)
      conformaldata_test$PTS_Lower90 = max(0, conformaldata_test$PTS_prediction - (conformaldata_test$PTS_std * quantile_PTS))
      conformaldata_test$PTS_Upper90 = conformaldata_test$PTS_prediction + (conformaldata_test$PTS_std * quantile_PTS)
      
      conformaldata_test$CET_covered = ifelse(conformaldata_test$CET_Lower90 < conformaldata_test$actual.x & conformaldata_test$CET_Upper90 > conformaldata_test$actual.x, 1, 0)
      conformaldata_test$PTS_covered = ifelse(conformaldata_test$PTS_Lower90 < conformaldata_test$actual.total.spending & conformaldata_test$PTS_Upper90 > conformaldata_test$actual.total.spending, 1, 0)
      
      # Fill the validity table for this column/run
      CET_validity_table = merge(x = CET_validity_table, y = conformaldata_test[, c("Id", "CET_covered")], by = "Id", all.x = TRUE)
      names(CET_validity_table)[length(names(CET_validity_table))] = paste("run", toString(length(names(CET_validity_table))))
      PTS_validity_table = merge(x = PTS_validity_table, y = conformaldata_test[, c("Id", "PTS_covered")], by = "Id", all.x = TRUE)
      names(PTS_validity_table)[length(names(PTS_validity_table))] = paste("run", toString(length(names(PTS_validity_table))))
      
      # Transfer the results to LowerPI and UpperPI
      CET_LowerPI = merge(x = CET_LowerPI, y = conformaldata_test[,c("Id", "CET_Lower90")], by = "Id", all.x = TRUE)
      CET_UpperPI = merge(x = CET_UpperPI, y = conformaldata_test[,c("Id", "CET_Upper90")], by = "Id", all.x = TRUE)
      PTS_LowerPI = merge(x = PTS_LowerPI, y = conformaldata_test[,c("Id", "PTS_Lower90")], by = "Id", all.x = TRUE)
      PTS_UpperPI = merge(x = PTS_UpperPI, y = conformaldata_test[,c("Id", "PTS_Upper90")], by = "Id", all.x = TRUE)
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
      parametertable[i, 6] = mean(conformaldata_test$CET_covered)
      parametertable[i, 7] = mean(conformaldata_test$PTS_covered)
      parametertable[i, 8] = as.numeric(quantile_CET)
      parametertable[i, 9] = as.numeric(quantile_PTS)
    },
    # Catch errors
    error = function(e){},
    warning = function(w){}
  )
}

# Create the resulting table for comparison with other methods
intervals_CP = data.table("Id" = customers,
                          "CET_Prediction" = results$CET,
                          "PTS_Prediction" = results$predicted.total.spending,
                          "CET_Lower" = CET_LowerPI$mean,
                          "CET_Upper" = CET_UpperPI$mean,
                          "CET_true" = results$actual.x,
                          "PTS_Lower" = PTS_LowerPI$mean,
                          "PTS_Upper" = PTS_UpperPI$mean,
                          "PTS_true" = results$actual.total.spending,
                          "CET_covered" = CET_LowerPI$mean < results$actual.x & CET_UpperPI$mean > results$actual.x,
                          "PTS_covered" = PTS_LowerPI$mean < results$actual.total.spending & PTS_UpperPI$mean > results$actual.total.spending)

###################
# From here analysis of the performance, not directly part of Conformal prediction

# Performance
sum(intervals_CP$CET_covered)/250
sum(intervals_CP$PTS_covered)/250

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

############################

val = data.table("run" = seq(1,length(CET_validities),1),
                 "PTS_validities" = CET_validities,
                 "average" = 0)
for (i in 1:length(PTS_validities)){
  val[i,3] = mean(as.vector(unlist(val[1:i,2])))
}

ggplot(val, aes(x = run, y = average)) +
  geom_line() #+
  geom_point(aes(y = PTS_validities))

