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

# Estimate standard Pareto/NBD Model

# Split data set into training, calibration and testing
train = sample(unique(apparelTrans$Id), 80)
at1 = apparelTrans
at1$class = ifelse(at1$Id %in% train, "train", "")
atTrainData = at1[class == "train",]
length(unique(atTrainData$Id))
at1 = at1[!class == "train",]
calibrate = sample(unique(at1$Id), 80)
at1$class = ifelse(at1$Id %in% calibrate, "calibrate", "test")
atCalibrateData = at1[class == "calibrate"]
length(unique(atCalibrateData$Id))
atTestData = at1[class == "test"]

# convert the data clvdata
atTrain = clvdata(atTrainData,
                  date.format="ymd", 
                  time.unit = "week",
                  estimation.split = splitWeek,
                  name.id = "Id",
                  name.date = "Date",
                  name.price = "Price")

atCalibrate = clvdata(atCalibrateData,
                  date.format="ymd",
                  time.unit = "week",
                  estimation.split = splitWeek,
                  name.id = "Id",
                  name.date = "Date",
                  name.price = "Price")

atTest = clvdata(atTestData,
                      date.format="ymd",
                      time.unit = "week",
                      estimation.split = splitWeek,
                      name.id = "Id",
                      name.date = "Date",
                      name.price = "Price")

# Fit the models
trainModelpnbd = pnbd(clv.data = atTrain)
calibrateModelpnbd = pnbd(clv.data = atCalibrate)
testModelpnbd = pnbd(clv.data = atTest)
trainModelgg = gg(clv.data = atTrain)
calibrateModelgg = gg(clv.data = atCalibrate)
testModelgg = gg(clv.data = atTest)


# Take parameters for the calibration models from the train models
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

# Conformal prediction for calibration data
conformal_data_calibrate = data.table("Id" = results_pnbd_calibrate$Id,
                            "Prediction" = PredCalibrate,
                            "True" = PredCalibrate + (PredCalibrate * runif(length(PredCalibrate), -0.2, 0.2)))

conformal_data_calibrate$error = abs(conformal_data_calibrate$Prediction - conformal_data_calibrate$True)/conformal_data_calibrate$Prediction
quantile = quantile(conformal_data_calibrate$error, 0.95)
hist(conformal_data_calibrate$error, breaks = 20)
conformal_data_calibrate$Lower90 = conformal_data_calibrate$Prediction - (conformal_data_calibrate$Prediction * quantile)
conformal_data_calibrate$Upper90 = conformal_data_calibrate$Prediction + (conformal_data_calibrate$Prediction * quantile)
sum(conformal_data_calibrate$True < conformal_data_calibrate$Upper90 & conformal_data_calibrate$True > conformal_data_calibrate$Lower90) / nrow(conformal_data_calibrate)

# Conformal prediction for test data
conformaldata_test = data.table("Id" = results_pnbd_test$Id,
                                "Prediction" = PredTest,
                                "True" = PredTest + (PredTest * runif(length(PredTest), -0.2, 0.2)))

conformaldata_test$Lower90 = conformaldata_test$Prediction - (conformaldata_test$Prediction * quantile)
conformaldata_test$Upper90 = conformaldata_test$Prediction + (conformaldata_test$Prediction * quantile)
sum(conformaldata_test$True < conformaldata_test$Upper90 & conformaldata_test$True > conformaldata_test$Lower90) / nrow(conformaldata_test)





