# 1. Get the variances
# 2. Get the intervals (acadmeic version) and collect the quantiles (managerial version)

# Set default settings
# splitweek = 130
ntraining = 2300
ntest = 4643 - ntraining
customers = unique(el1$Id)

clv.gifts <- clvdata(el1,
                     date.format="ymd",
                     time.unit = "week",
                     estimation.split = splitweek1,
                     name.id = "Id",
                     name.date = "Date",
                     name.price = "Price")
est.pnbd = pnbd(clv.data = clv.gifts)

if (end1 > 0){
  results.el1 = predict(est.pnbd, predict.spending = TRUE, prediction.end = end1)
  print("a")
}else{
  results.el1 = predict(est.pnbd, predict.spending = TRUE)
  print("b")
}

# Table based on differences from a single fit
cor_table = data.table("Id" = customers,
                       "CET_pred" = results.el1$CET,
                       "CET_true" = results.el1$actual.x,
                       "CET_std" = abs(results.el1$actual.x - results.el1$CET),
                       "PTS_pred" = results.el1$predicted.total.spending,
                       "PTS_true" = results.el1$actual.total.spending,
                       "PTS_std" = abs(results.el1$actual.total.spending - results.el1$predicted.total.spending))

# Fit the linear models based on the 2. table
CET_mod = lm(CET_std ~ CET_pred, data = cor_table)
PTS_mod = lm(PTS_std ~ PTS_pred, data = cor_table)

# ggplot(cor_table,aes(PTS_pred, PTS_std)) +
#   stat_summary(fun.data=mean_cl_normal) + 
#   geom_smooth(method='lm', formula= y~x)

# Set default settings 
alpha = 0.1
q = ceiling(((ntraining + 1) * (1 - alpha)))/ntraining # see 9CP, equation 7


#########
# Split the data set in training, validation and test
smp = sample(customers, ntraining, replace = FALSE)
train = el1[Id %in% smp,]
test = el1[!(Id %in% smp),]


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
results_pnbd_train = predict(trainModelpnbd)
results_pnbd_test = predict(testModelpnbd)

if (end1 > 0){
  results_pnbd_train = predict(trainModelpnbd, prediction.end = end1)
  results_pnbd_test = predict(testModelpnbd, prediction.end = end1)
  print("a")
}else{
  results_pnbd_train = predict(trainModelpnbd)
  results_pnbd_test = predict(testModelpnbd)
  print("b")
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

quantile_CET_big = quantile(conformal_data_train$CET_error, q)
quantile_PTS_big = quantile(conformal_data_train$PTS_error, q)

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
conformal_data_test$CET_Lower90 = conformal_data_test$CET_prediction - (conformal_data_test$CET_std * quantile_CET_big)
conformal_data_test$CET_Upper90 = conformal_data_test$CET_prediction + (conformal_data_test$CET_std * quantile_CET_big)
conformal_data_test$PTS_Lower90 = conformal_data_test$PTS_prediction - (conformal_data_test$PTS_std * quantile_PTS_big)
conformal_data_test$PTS_Upper90 = conformal_data_test$PTS_prediction + (conformal_data_test$PTS_std * quantile_PTS_big)

# Calculate the coverage
conformal_data_test$CET_covered = ifelse(conformal_data_test$CET_Lower90 < conformal_data_test$actual.x & conformal_data_test$CET_Upper90 > conformal_data_test$actual.x, 1, 0)
conformal_data_test$PTS_covered = ifelse(conformal_data_test$PTS_Lower90 < conformal_data_test$actual.total.spending & conformal_data_test$PTS_Upper90 > conformal_data_test$actual.total.spending, 1, 0)

# Create the resulting table for comparison with other methods
intervals_CP = data.table("Id" = results.el1$Id,
                            "CET_lower" = max(0, CET_LowerPI$mean),
                            "CET_upper" = CET_UpperPI$mean,
                            "CET_true" = results.el1$actual.x,
                            "CET_prediction" = results.el1$CET,
                            "CET_covered" = CET_LowerPI$mean < results.el1$actual.x & CET_UpperPI$mean > results.el1$actual.x,
                            "PTS_lower" = max(0, PTS_LowerPI$mean),
                            "PTS_upper" = PTS_UpperPI$mean,
                            "PTS_true" = results.el1$actual.total.spending,
                            "PTS_prediction" = results.el1$predicted.total.spending,
                            "PTS_covered" = PTS_LowerPI$mean < results.el1$actual.total.spending & PTS_UpperPI$mean > results.el1$actual.total.spending
)