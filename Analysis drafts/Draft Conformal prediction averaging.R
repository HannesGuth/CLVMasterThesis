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
LowerPI = data.table("Id" = customers,
                     "mean" = NA)
UpperPI = data.table("Id" = customers,
                     "mean" = NA)
quantiles = list()
q = ceiling(((ncalibration + 1) * (1 - alpha)))/ncalibration
true = data.table("Id" = results$"Id",
                  #"True" = results$predicted.CLV + (results$predicted.CLV * runif(length(results$predicted.CLV), -0.2, 0.2)))
                  "True" = new3$CLV)
validity_table = data.table("Id" = customers)
parametertable = data.table("run" = seq(1:200),
                            "r" = 0,
                            "a" = 0,
                            "s" = 0,
                            "b" = 0,
                            "accuracy" = 0,
                            "quantile" = 0)
# Train many models, use CP to predict intervals, average over the results
for (i in 1:80){
#while (sum(is.na(LowerPI$mean)) > 0){
  print(sum(is.na(LowerPI$mean)))
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
      
      # Calculate the CLV t´with the predicted DERT and predicted mean spending
      PredCalibrate = results_pnbd_calibrate$DERT * results_gg_calibrate$predicted.mean.spending
      PredTest = results_pnbd_test$DERT * results_gg_test$predicted.mean.spending
      
      # Create a table that later contains Id, Prediction, True values, error and quantiles for the calibration data
      conformal_data_calibrate = data.table("Id" = results_pnbd_calibrate$Id,
                                            "Prediction" = PredCalibrate)
      conformal_data_calibrate = merge(conformal_data_calibrate, true, by = "Id")
      
      # Get the rror quantiles in this run
      conformal_data_calibrate$error = abs(conformal_data_calibrate$Prediction - conformal_data_calibrate$True)/conformal_data_calibrate$Prediction
      quantile = quantile(conformal_data_calibrate$error, q)
      if (quantile == 1) next
      quantiles = append(quantiles, quantile)
      
      # Predict the intervals in this run for the calibration data (actually not needed)
      conformal_data_calibrate$Lower90 = conformal_data_calibrate$Prediction - (conformal_data_calibrate$Prediction * quantile)
      conformal_data_calibrate$Upper90 = conformal_data_calibrate$Prediction + (conformal_data_calibrate$Prediction * quantile)
      
      # Create a table for the test data that contains the PIs for each customer in this run and if the true value is covered
      conformaldata_test = data.table("Id" = results_pnbd_test$Id,
                                      "Prediction" = PredTest)
      conformaldata_test = merge(x = conformaldata_test, y = true, by = "Id")
      conformaldata_test$Lower90 = conformaldata_test$Prediction - (conformaldata_test$Prediction * quantile)
      conformaldata_test$Upper90 = conformaldata_test$Prediction + (conformaldata_test$Prediction * quantile)
      conformaldata_test$covered = ifelse(conformaldata_test$Lower90 < conformaldata_test$True & conformaldata_test$Upper90 > conformaldata_test$True, 1, 0)
      
      # Fill the validity table for this column/run
      validity_table = merge(x = validity_table, y = conformaldata_test[, c("Id", "covered")], by = "Id", all.x = TRUE)
      names(validity_table)[length(names(validity_table))] = paste("run", toString(length(names(validity_table))))
      
      # Transfer the results to LowerPI and UpperPI
      LowerPI = merge(x = LowerPI, y = conformaldata_test[,c("Id", "Lower90")], by = "Id", all.x = TRUE)
      UpperPI = merge(x = UpperPI, y = conformaldata_test[,c("Id", "Upper90")], by = "Id", all.x = TRUE)
      names(LowerPI)[length(names(LowerPI))] = paste(toString(length(names(LowerPI))))
      names(UpperPI)[length(names(LowerPI))] = paste(toString(length(names(LowerPI))))
      LowerPI$mean = rowMeans(LowerPI[,!c("Id","mean")], na.rm = TRUE)
      UpperPI$mean = rowMeans(UpperPI[,!c("Id","mean")], na.rm = TRUE)
      
      parametertable[i, 2] = trainModelpnbd@prediction.params.model[1]
      parametertable[i, 3] = trainModelpnbd@prediction.params.model[2]
      parametertable[i, 4] = trainModelpnbd@prediction.params.model[3]
      parametertable[i, 5] = trainModelpnbd@prediction.params.model[4]
      parametertable[i, 6] = mean(conformaldata_test$covered)
      parametertable[i, 7] = as.numeric(quantile)
    },
    # Catch errors
    error = function(e){},
    warning = function(w){}
  )
}

# Create the resulting table for comparison with other methods
intervals_CP = data.table("Id" = customers,
                          "Prediction" = results$predicted.CLV,
                          "True" = true$True,
                          "Lower" = LowerPI$mean,
                          "Upper" = UpperPI$mean)

# Measure validity
mean(as.vector(unlist(validity_table[,2:length(validity_table)])), na.rm = TRUE)

validities = list()
for (i in 2:length(validity_table)){
  validities = append(validities, mean(unlist(validity_table[,..i]), na.rm = TRUE))
  print(paste("i", i, ": ", mean(unlist(validity_table[,..i]), na.rm = TRUE)))
}
validities = as.vector(unlist(validities))
hist(validities)
plot(density(validities))
