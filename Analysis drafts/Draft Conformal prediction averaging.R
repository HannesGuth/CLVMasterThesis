install.packages("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/CLVTools", repos = NULL, type="source")

# Load package
library(CLVTools)
library(data.table)
library(compiler)
library(ggplot2)
library(profvis)
library(rockchalk)
library(TAF)

splitWeek = 40

# Load data
data("apparelTrans")
clv.apparel <- clvdata(apparelTrans,  
                       date.format="ymd", 
                       time.unit = "week",
                       estimation.split = splitWeek,
                       name.id = "Id",
                       name.date = "Date",
                       name.price = "Price")
customers = unique(aptr$Id)
LowerPI = data.table("Id" = customers)
UpperPI = data.table("Id" = customers)

true = data.table("Id" = results$"Id",
                  "True" = results$predicted.CLV + (results$predicted.CLV * runif(length(results$predicted.CLV), -0.2, 0.2)))

for (i in 1:20){
  smp = sample(customers, 70, replace = FALSE)
  test = apparelTrans[Id %in% smp,]
  trca = apparelTrans[!(Id %in% smp),]
  ids = sample(unique(trca$Id), 100)
  trca$class = ifelse(trca$Id %in% ids, "train", "calibrate")
  train = trca[class == "train",]
  calibrate = trca[class == "calibrate",]
  
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
  tryCatch(
    {
    trainModelpnbd = pnbd(clv.data = trainCLV)
    calibrateModelpnbd = pnbd(clv.data = calibrateCLV)
    testModelpnbd = pnbd(clv.data = testCLV)
    trainModelgg = gg(clv.data = trainCLV)
    calibrateModelgg = gg(clv.data = calibrateCLV)
    testModelgg = gg(clv.data = testCLV)
    #testModelpnbd
    },
    warning = function(w){},
    error = function(e){}
  )
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
  
  results_pnbd_calibrate = predict(calibrateModelpnbd)
  results_gg_calibrate = predict(calibrateModelgg)
  results_pnbd_test = predict(testModelpnbd)
  results_gg_test = predict(testModelgg)
  
  PredCalibrate = results_pnbd_calibrate$DERT * results_gg_calibrate$predicted.mean.spending
  PredTest = results_pnbd_test$DERT * results_gg_test$predicted.mean.spending
  
  conformal_data_calibrate = data.table("Id" = results_pnbd_calibrate$Id,
                                        "Prediction" = PredCalibrate)
  conformal_data_calibrate = merge(conformal_data_calibrate, true, by = "Id")
  
  conformal_data_calibrate$error = abs(conformal_data_calibrate$Prediction - conformal_data_calibrate$True)/conformal_data_calibrate$Prediction
  quantile = quantile(conformal_data_calibrate$error, 0.95)
  conformal_data_calibrate$Lower90 = conformal_data_calibrate$Prediction - (conformal_data_calibrate$Prediction * quantile)
  conformal_data_calibrate$Upper90 = conformal_data_calibrate$Prediction + (conformal_data_calibrate$Prediction * quantile)
  
  conformaldata_test = data.table("Id" = results_pnbd_test$Id,
                                  "Prediction" = PredTest)

  conformaldata_test$Lower90 = conformaldata_test$Prediction - (conformaldata_test$Prediction * quantile)
  conformaldata_test$Upper90 = conformaldata_test$Prediction + (conformaldata_test$Prediction * quantile)

  LowerPI = merge(x = LowerPI, y = conformaldata_test[,c(1,3)], by = "Id", all.x = TRUE)
  UpperPI = merge(x = UpperPI, y = conformaldata_test[,c(1,4)], by = "Id", all.x = TRUE)
  names(LowerPI)[length(names(LowerPI))] = paste(toString(i))
  names(UpperPI)[length(names(LowerPI))] = paste(toString(i))
}


LowerPI$mean = rowMeans(LowerPI[,2:20], na.rm = TRUE)
UpperPI$mean = rowMeans(UpperPI[,2:20], na.rm = TRUE)

summary_table = data.table("Prediction" = results$predicted.CLV,
                           "True" = true$True,
                           "Lower" = LowerPI$mean,
                           "Upper" = UpperPI$mean)
summary_table = summary_table[complete.cases(summary_table),]
sum(summary_table$True < summary_table$Upper & summary_table$True > summary_table$Lower) / nrow(summary_table)
