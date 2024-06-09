# install.packages("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/CLVTools", repos = NULL, type="source")
# 
# # Load package
# library(CLVTools)
# library(data.table)
# library(compiler)
# library(ggplot2)
# library(profvis)
# library(rockchalk)
# library(TAF)
# 
# splitWeek = 40
# 
# # Load data
# data("apparelTrans")
# clv.apparel <- clvdata(apparelTrans,
#                        date.format="ymd", 
#                        time.unit = "week",
#                        estimation.split = splitWeek,
#                        name.id = "Id",
#                        name.date = "Date",
#                        name.price = "Price")
# 
# # Predict intervals and calculate validity
# 
# # Initialize variables and tables for the CP process
# est.pnbd = pnbd(clv.data = clv.apparel)
# results = predict(est.pnbd, predict.spending = TRUE)

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
q = ceiling(((ncalibration + 1) * (1 - alpha)))/ncalibration
true = data.table("Id" = results$"Id",
                  #"True" = results$predicted.CLV + (results$predicted.CLV * runif(length(results$predicted.CLV), -0.2, 0.2)))
                  "True" = new3$CLV)
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
#while (sum(is.na(LowerPI$mean)) > 0){
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
      trainModelgg = gg(clv.data = trainCLV)
      calibrateModelgg = gg(clv.data = calibrateCLV)
      testModelgg = gg(clv.data = testCLV)
      
      # Take the parameters from the training model for the other 2 models as well (only like this, it is possible
      # to make predictions for calibration and test data using the train model)
      calibrateModelpnbd@prediction.params.model[1] = trainModelpnbd@prediction.params.model[1]
      calibrateModelpnbd@prediction.params.model[2] = trainModelpnbd@prediction.params.model[2]
      calibrateModelpnbd@prediction.params.model[3] = trainModelpnbd@prediction.params.model[3]
      calibrateModelpnbd@prediction.params.model[4] = trainModelpnbd@prediction.params.model[4]
      
      calibrateModelgg@prediction.params.model[1] = trainModelgg@prediction.params.model[1]
      calibrateModelgg@prediction.params.model[2] = trainModelgg@prediction.params.model[2]
      calibrateModelgg@prediction.params.model[3] = trainModelgg@prediction.params.model[3]
      
      testModelpnbd@prediction.params.model[1] = trainModelpnbd@prediction.params.model[1]
      testModelpnbd@prediction.params.model[2] = trainModelpnbd@prediction.params.model[2]
      testModelpnbd@prediction.params.model[3] = trainModelpnbd@prediction.params.model[3]
      testModelpnbd@prediction.params.model[4] = trainModelpnbd@prediction.params.model[4]
      
      testModelgg@prediction.params.model[1] = trainModelgg@prediction.params.model[1]
      testModelgg@prediction.params.model[2] = trainModelgg@prediction.params.model[2]
      testModelgg@prediction.params.model[3] = trainModelgg@prediction.params.model[3]
      
      # Make predictions
      results_pnbd_calibrate = predict(calibrateModelpnbd)
      results_gg_calibrate = predict(calibrateModelgg)
      results_pnbd_test = predict(testModelpnbd)
      results_gg_test = predict(testModelgg)
      
      
      # Create a table that later contains Id, Prediction, True values, error and quantiles for the calibration data
      conformal_data_calibrate = data.table("Id" = results_pnbd_calibrate$Id,
                                            "CET_prediction" = results_pnbd_calibrate$CET,
                                            "PTS_prediction" = results_pnbd_calibrate$predicted.total.spending,
                                            "CET_true" = results_pnbd_calibrate$actual.x,
                                            "PTS_true" = results_pnbd_calibrate$actual.total.spending
      )
      
      # Get the error quantiles in this run
      conformal_data_calibrate$CET_error = abs(conformal_data_calibrate$CET_prediction - conformal_data_calibrate$CET_true)/conformal_data_calibrate$CET_prediction
      conformal_data_calibrate$PTS_error = abs(conformal_data_calibrate$PTS_prediction - conformal_data_calibrate$PTS_true)/conformal_data_calibrate$PTS_prediction
      quantile_CET = quantile(conformal_data_calibrate$CET_error, q)
      quantile_PTS = quantile(conformal_data_calibrate$PTS_error, q)
      if (quantile_PTS == 1 | quantile_CET == 1) next
      quantiles_CET = append(quantiles_CET, quantile_CET)
      quantiles_PTS = append(quantiles_PTS, quantile_PTS)
      
      # Predict the intervals in this run for the calibration data (actually not needed)
      conformal_data_calibrate$CET_Lower90 = conformal_data_calibrate$CET_prediction - (conformal_data_calibrate$CET_prediction * quantile_CET)
      conformal_data_calibrate$CET_Upper90 = conformal_data_calibrate$CET_prediction + (conformal_data_calibrate$CET_prediction * quantile_CET)
      conformal_data_calibrate$PTS_Lower90 = conformal_data_calibrate$PTS_prediction - (conformal_data_calibrate$PTS_prediction * quantile_PTS)
      conformal_data_calibrate$PTS_Upper90 = conformal_data_calibrate$PTS_prediction + (conformal_data_calibrate$PTS_prediction * quantile_PTS)
      
      # Create a table for the test data that contains the PIs for each customer in this run and if the true value is covered
      conformaldata_test = data.table("Id" = results_pnbd_test$Id,
                                      "CET_prediction" = results_pnbd_test$CET,
                                      "PTS_prediction" = results_pnbd_test$predicted.total.spending
      )
      
      conformaldata_test = merge(x = conformaldata_test, y = results[,c("Id", "actual.x", "actual.total.spending")], by = "Id")
      conformaldata_test$CET_Lower90 = conformaldata_test$CET_prediction - (conformaldata_test$CET_prediction * quantile_CET)
      conformaldata_test$CET_Upper90 = conformaldata_test$CET_prediction + (conformaldata_test$CET_prediction * quantile_CET)
      conformaldata_test$PTS_Lower90 = conformaldata_test$PTS_prediction - (conformaldata_test$PTS_prediction * quantile_PTS)
      conformaldata_test$PTS_Upper90 = conformaldata_test$PTS_prediction + (conformaldata_test$PTS_prediction * quantile_PTS)
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
                          "PTS_Lower" = PTS_LowerPI$mean,
                          "PTS_Upper" = PTS_UpperPI$mean)

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
