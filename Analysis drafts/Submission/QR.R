# splitweek = 130

# data1
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


# Create a grid where parameters are expected to be to achieve the desired quantiles (pre-knowledge helpful to reduce computational effort)
grid = expand.grid(c(seq(0.01,0.08,0.01), seq(2.3,3.5,0.3)),
                   c(seq(3500,3600,10), seq(50,90,10)),
                   c(seq(1,10,1), seq(0.000008,0.00003,0.000005)),
                   100)
grid = cbind(i = seq(1, nrow(grid)), grid)
grid$maxCET = 0
grid$maxPTS = 0
grid$maxDiff = 0
grid$minCET = 0
grid$minPTS = 0
grid$minDiff = 0
grid$CET_theta_U = 0
grid$CET_theta_L = 0
grid$PTS_theta_U = 0
grid$PTS_theta_L = 0


#alpha = 0.1
CET_tolerance = 0.1
PTS_tolerance = 1.5
best_CET_i = 0
best_PTS_i = 0
CET_diffsummax = Inf
PTS_diffsummax = Inf
CET_diffsummin = Inf
PTS_diffsummin = Inf

# Set up the parallel backend
num_cores <- parallel::detectCores()
print("1")
cl <- makeCluster(num_cores - 2)
print("2")
registerDoParallel(cl)
print("3")

# Define the model fitting function
fit_model <- function(grid, i, est.data1) {
  print(i)
  est.data1@prediction.params.model[1] = as.numeric(grid[i,2])
  est.data1@prediction.params.model[2] = as.numeric(grid[i,3])
  est.data1@prediction.params.model[3] = as.numeric(grid[i,4])
  est.data1@prediction.params.model[4] = as.numeric(grid[i,5])
  if (whole_period1){
    pred = predict(est.data1, predict.spending = TRUE)
  }else{
    pred = predict(est.data1, predict.spending = TRUE, prediction.end = end1)
  }
  
  pred$CET = (pred$CET <= CET_tolerance) * 0 + (pred$CET > CET_tolerance) * pred$CET
  pred$predicted.total.spending = (pred$predicted.total.spending <= PTS_tolerance) * 0 + (pred$predicted.total.spending > PTS_tolerance) * pred$predicted.total.spending
  
  # "Loss functions"
  # Upper
    # Direct (not selected at the moment)
    CET_diffsummax = (sum(pred$actual.x >= pred$CET)/nrow(pred)) - alpha/2
    PTS_diffsummax = (sum(pred$actual.total.spending >= pred$predicted.total.spending)/nrow(pred)) - alpha/2
    # With formula (selected at the moment)
    # CET_thetamax = sum((pred$actual.x >= pred$CET) * (1 - alpha) + (pred$actual.x < pred$CET) * alpha)/nrow(pred) - alpha
    # PTS_thetamax = sum((pred$actual.total.spending >= pred$predicted.total.spending) * (1 - alpha) + (pred$actual.total.spending < pred$predicted.total.spending) * alpha)/nrow(pred) - alpha
  
  # Lower
    # Direct (not selected at the moment)
    CET_diffsummin = (sum(pred$actual.x <= pred$CET)/nrow(pred)) - alpha/2
    PTS_diffsummin = (sum(pred$actual.total.spending <= pred$predicted.total.spending)/nrow(pred)) - alpha/2
    # With formula (selected at the moment)
    # CET_thetamin = sum((pred$actual.x + CET_tolerance < pred$CET) * (1 - alpha) + (pred$actual.x >= pred$CET) * alpha)/nrow(pred) - alpha
    # PTS_thetamin = sum((pred$actual.total.spending + PTS_tolerance < pred$predicted.total.spending) * (1 - alpha) + (pred$actual.total.spending >= pred$predicted.total.spending) * alpha)/nrow(pred) - alpha
  
  grid[i,6] = CET_diffsummax
  grid[i,7] = PTS_diffsummax
  grid[i,8] = abs(CET_diffsummax) + abs(PTS_diffsummax)
  
  grid[i,9] = CET_diffsummin
  grid[i,10] = PTS_diffsummin
  grid[i,11] = abs(CET_diffsummin) + abs(PTS_diffsummin)
  
  # grid[i,12] = CET_thetamax
  # grid[i,13] = CET_thetamin
  # grid[i,14] = PTS_thetamax
  # grid[i,15] = PTS_thetamin
  
  #grid[14] = abs(CET_diffsummin) + abs(PTS_diffsummin)
  # Collect data
  return(c(grid[i,1], grid[i,2], grid[i,3], grid[i,4], grid[i,5], grid[i,6], grid[i,7], grid[i,8], grid[i,9], grid[i,10], grid[i,11], grid[i,12], grid[i,13], grid[i,14], grid[i,15]))
}

print("4")
# Run the loop in parallel
result = foreach(i = 1:nrow(grid), .combine = rbind, .packages = "stats") %dopar% {
  fit_model(grid, i, est.data1)
}
print("5")
stopCluster(cl)
print("6")
# Transfer results and rename columns
grid = data.table(result)
print("7")
colnames(grid) = c("i", "r", "alpha", "s", "beta", "CET_U_diff", "PTS_U_diff", "U_diff", "CET_L_diff", "PTS_L_diff", "L_diff", "CET_t_Udiff", "CET_t_Ldiff", "PTS_t_Udiff", "PTS_t_Ldiff")



# Evaluation (selected, takes the formula approach, not the direct approach)
# Get the intervals

index_list = list()

# Upper CET
min(abs(grid$CET_U_diff), na.rm = TRUE)
grid[abs(CET_U_diff) <= min(abs(grid$CET_U_diff), na.rm = TRUE)]
i = as.numeric(grid[abs(CET_U_diff) <= min(abs(grid$CET_U_diff), na.rm = TRUE)][1,1])
index_list = append(index_list, i)
est.data1@prediction.params.model[1] = as.numeric(grid[i,2])
est.data1@prediction.params.model[2] = as.numeric(grid[i,3])
est.data1@prediction.params.model[3] = as.numeric(grid[i,4])
est.data1@prediction.params.model[4] = as.numeric(grid[i,5])
if (whole_period1){
  pred = predict(est.data1, predict.spending = TRUE)
}else{
  pred = predict(est.data1, predict.spending = TRUE, prediction.end = end1)
}
#pred
sum(pred$actual.x >= pred$CET)/nrow(pred) - alpha/2
CET_upper = pred$CET


# Lower CET
min(abs(grid$CET_L_diff), na.rm = TRUE)
grid[abs(CET_L_diff) <= min(abs(grid$CET_L_diff), na.rm = TRUE)]
i = as.numeric(grid[abs(CET_L_diff) <= min(abs(grid$CET_L_diff), na.rm = TRUE)][1,1])
index_list = append(index_list, i)
est.data1@prediction.params.model[1] = as.numeric(grid[i,2])
est.data1@prediction.params.model[2] = as.numeric(grid[i,3])
est.data1@prediction.params.model[3] = as.numeric(grid[i,4])
est.data1@prediction.params.model[4] = as.numeric(grid[i,5])
if (whole_period1){
  pred = predict(est.data1, predict.spending = TRUE)
}else{
  pred = predict(est.data1, predict.spending = TRUE, prediction.end = end1)
}
pred$CET = (pred$CET < CET_tolerance) * 0 + (pred$CET > CET_tolerance) * pred$CET
sum(pred$actual.x <= pred$CET)/nrow(pred)
pred$ok = pred$actual.x <= pred$CET
CET_lower = pred$CET

# Upper PTS
min(abs(grid$PTS_U_diff), na.rm = TRUE)
grid[PTS_U_diff <= min(abs(grid$PTS_U_diff), na.rm = TRUE)]
i = as.numeric(grid[abs(PTS_U_diff) <= min(abs(grid$PTS_U_diff), na.rm = TRUE)][1,1])
index_list = append(index_list, i)
est.data1@prediction.params.model[1] = as.numeric(grid[i,2])
est.data1@prediction.params.model[2] = as.numeric(grid[i,3])
est.data1@prediction.params.model[3] = as.numeric(grid[i,4])
est.data1@prediction.params.model[4] = as.numeric(grid[i,5])
if (whole_period1){
  pred = predict(est.data1, predict.spending = TRUE)
}else{
  pred = predict(est.data1, predict.spending = TRUE, prediction.end = end1)
}
sum(pred$actual.total.spending >= pred$predicted.total.spending)/nrow(pred)
PTS_upper = pred$predicted.total.spending

# Lower PTS
min(abs(grid$PTS_L_diff), na.rm = TRUE)
grid[abs(PTS_L_diff) <= min(abs(grid$PTS_L_diff), na.rm = TRUE)]
i = as.numeric(grid[PTS_L_diff <= min(abs(grid$PTS_L_diff), na.rm = TRUE)][1,1])
index_list = append(index_list, i)
est.data1@prediction.params.model[1] = as.numeric(grid[i,2])
est.data1@prediction.params.model[2] = as.numeric(grid[i,3])
est.data1@prediction.params.model[3] = as.numeric(grid[i,4])
est.data1@prediction.params.model[4] = as.numeric(grid[i,5])
if (whole_period1){
  pred = predict(est.data1, predict.spending = TRUE)
}else{
  pred = predict(est.data1, predict.spending = TRUE, prediction.end = end1)
}
pred$ok = pred$actual.total.spending <= pred$predicted.total.spending
pred$predicted.total.spending = (pred$predicted.total.spending < CET_tolerance) * 0 + (pred$predicted.total.spending > CET_tolerance) * pred$predicted.total.spending
sum(pred$actual.total.spending <= pred$predicted.total.spending)/nrow(pred)
PTS_lower = pred$predicted.total.spending

intervals_QR = data.table("Id" = pred$Id,
                          "CET_lower" = round(CET_lower, 4),
                          "CET_upper" = CET_upper,
                          "CET_true" = pred$actual.x,
                          "CET_prediction" = results.data1$CET,
                          "CET_covered" = results.data1$actual.x >= CET_lower & results.data1$actual.x < CET_upper,
                          "PTS_lower" = round(PTS_lower, 4),
                          "PTS_upper" = PTS_upper,
                          "PTS_true" = results.data1$actual.total.spending,
                          "PTS_pred" = results.data1$predicted.total.spending,
                          "PTS_covered" = results.data1$actual.total.spending >= PTS_lower & results.data1$actual.total.spending < PTS_upper)

mean(intervals_QR$CET_covered)
mean(intervals_QR$PTS_covered)

######### Managerial version
clv.data2 <- clvdata(data2,
                     date.format="ymd", 
                     time.unit = "week",
                     estimation.split = splitweek2,
                     name.id = "Id",
                     name.date = "Date",
                     name.price = "Price")
est.data2 = pnbd(clv.data = clv.data2)
if(whole_period2){
  results.data2 = predict(est.data2, predict.spending = TRUE)
}else{
  results.data2 = predict(est.data2, predict.spending = TRUE, prediction.end = end2)
}

# CET upper
est.data2@prediction.params.model[1] = as.numeric(grid[as.numeric(index_list[1]), 2])
est.data2@prediction.params.model[2] = as.numeric(grid[as.numeric(index_list[1]), 3])
est.data2@prediction.params.model[3] = as.numeric(grid[as.numeric(index_list[1]), 4])
est.data2@prediction.params.model[4] = as.numeric(grid[as.numeric(index_list[1]), 5])
if (whole_period2){
  pred = predict(est.data2, predict.spending = TRUE)
}else{
  pred = predict(est.data2, predict.spending = TRUE, prediction.end = end2)
}
sum(pred$actual.x >= pred$CET)/nrow(pred)
CET_upper = pred$CET

# CET lower
est.data2@prediction.params.model[1] = as.numeric(grid[as.numeric(index_list[2]), 2])
est.data2@prediction.params.model[2] = as.numeric(grid[as.numeric(index_list[2]), 3])
est.data2@prediction.params.model[3] = as.numeric(grid[as.numeric(index_list[2]), 4])
est.data2@prediction.params.model[4] = as.numeric(grid[as.numeric(index_list[2]), 5])
if (whole_period2){
  pred = predict(est.data2, predict.spending = TRUE)
}else{
  pred = predict(est.data2, predict.spending = TRUE, prediction.end = end2)
}
pred$CET = (pred$CET < CET_tolerance) * 0 + (pred$CET > CET_tolerance) * pred$CET
sum(pred$actual.x <= pred$CET)/nrow(pred)
CET_lower = pred$CET

# PTS upper
est.data2@prediction.params.model[1] = as.numeric(grid[as.numeric(index_list[3]), 2])
est.data2@prediction.params.model[2] = as.numeric(grid[as.numeric(index_list[3]), 3])
est.data2@prediction.params.model[3] = as.numeric(grid[as.numeric(index_list[3]), 4])
est.data2@prediction.params.model[4] = as.numeric(grid[as.numeric(index_list[3]), 5])
if (whole_period2){
  pred = predict(est.data2, predict.spending = TRUE)
}else{
  pred = predict(est.data2, predict.spending = TRUE, prediction.end = end2)
}
sum(pred$actual.total.spending >= pred$predicted.total.spending)/nrow(pred)
PTS_upper = pred$predicted.total.spending

# PTS lower
est.data2@prediction.params.model[1] = as.numeric(grid[as.numeric(index_list[4]), 2])
est.data2@prediction.params.model[2] = as.numeric(grid[as.numeric(index_list[4]), 3])
est.data2@prediction.params.model[3] = as.numeric(grid[as.numeric(index_list[4]), 4])
est.data2@prediction.params.model[4] = as.numeric(grid[as.numeric(index_list[4]), 5])
if (whole_period2){
  pred = predict(est.data2, predict.spending = TRUE)
}else{
  pred = predict(est.data2, predict.spending = TRUE, prediction.end = end2)
}
pred$predicted.total.spending = (pred$predicted.total.spending < CET_tolerance) * 0 + (pred$predicted.total.spending > CET_tolerance) * pred$predicted.total.spending
sum(pred$actual.total.spending <= pred$predicted.total.spending)/nrow(pred)
PTS_lower = pred$predicted.total.spending

intervals_QR_m = data.table("Id" = results_general$Id,
                          "CET_lower" = round(CET_lower, 4),
                          "CET_upper" = CET_upper,
                          "CET_true" = results_general$actual.x,
                          "CET_prediction" = results.data2$CET,
                          "CET_covered" = results.data2$actual.x >= CET_lower & results.data2$actual.x < CET_upper,
                          "PTS_lower" = round(PTS_lower, 4),
                          "PTS_upper" = PTS_upper,
                          "PTS_true" = results_general$actual.total.spending,
                          "PTS_prediction" = results.data2$predicted.total.spending,
                          "PTS_covered" = results.data2$actual.total.spending >= PTS_lower & results.data2$actual.total.spending < PTS_upper
)

mean(intervals_QR_m$CET_covered)
mean(intervals_QR_m$PTS_covered)

