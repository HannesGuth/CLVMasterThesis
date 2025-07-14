
# el1
clv.el1 <- clvdata(el1,  
                     date.format="ymd", 
                     time.unit = "week",
                     estimation.split = splitweek1,
                     name.id = "Id",
                     name.date = "Date",
                     name.price = "Price")
est.el1 = pnbd(clv.data = clv.el1)

if (end1 > 0){
  results.el1 = predict(est.el1, predict.spending = TRUE, prediction.end = end1)
}else{
  results.el1 = predict(est.el1, predict.spending = TRUE)
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


alpha = 0.05
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
cl <- makeCluster(num_cores - 2)
registerDoParallel(cl)

# Define the model fitting function
fit_model <- function(grid, i, est.el1) {
  print(i)
  est.el1@prediction.params.model[1] = as.numeric(grid[i,2])
  est.el1@prediction.params.model[2] = as.numeric(grid[i,3])
  est.el1@prediction.params.model[3] = as.numeric(grid[i,4])
  est.el1@prediction.params.model[4] = as.numeric(grid[i,5])
  
  if (end1 > 0){
    pred = predict(est.el1, predict.spending = TRUE, prediction.end = end1)
  }else{
    pred = predict(est.el1, predict.spending = TRUE)
  }
  
  # "Loss functions"
  # Upper
    # Direct (not selected at the moment)
    CET_diffsummax = (sum(pred$actual.x >= pred$CET)/nrow(pred)) - alpha
    PTS_diffsummax = (sum(pred$actual.total.spending >= pred$predicted.total.spending)/nrow(pred)) - alpha
    # With formula (selected at the moment)
    CET_thetamax = sum((pred$actual.x >= pred$CET) * (1 - alpha) + (pred$actual.x < pred$CET) * alpha)/nrow(pred) - alpha
    PTS_thetamax = sum((pred$actual.total.spending >= pred$predicted.total.spending) * (1 - alpha) + (pred$actual.total.spending < pred$predicted.total.spending) * alpha)/nrow(pred) - alpha
  
  # Lower
    # Direct (not selected at the moment)
    CET_diffsummin = (sum(pred$actual.x + CET_tolerance <= pred$CET)/nrow(pred)) - alpha
    PTS_diffsummin = (sum(pred$actual.total.spending + PTS_tolerance <= pred$predicted.total.spending)/nrow(pred)) - alpha
    # With formula (selected at the moment)
    CET_thetamin = sum((pred$actual.x + CET_tolerance < pred$CET) * (1 - alpha) + (pred$actual.x >= pred$CET) * alpha)/nrow(pred) - alpha
    PTS_thetamin = sum((pred$actual.total.spending + PTS_tolerance < pred$predicted.total.spending) * (1 - alpha) + (pred$actual.total.spending >= pred$predicted.total.spending) * alpha)/nrow(pred) - alpha
  
  grid[i,6] = CET_diffsummax
  grid[i,7] = PTS_diffsummax
  grid[i,8] = abs(CET_diffsummax) + abs(PTS_diffsummax)
  
  grid[i,9] = CET_diffsummin
  grid[i,10] = PTS_diffsummin
  grid[i,11] = abs(CET_diffsummin) + abs(PTS_diffsummin)
  
  grid[i,12] = CET_thetamax
  grid[i,13] = CET_thetamin
  grid[i,14] = PTS_thetamax
  grid[i,15] = PTS_thetamin
  
  #grid[14] = abs(CET_diffsummin) + abs(PTS_diffsummin)
  # Collect data
  return(c(grid[i,1], grid[i,2], grid[i,3], grid[i,4], grid[i,5], grid[i,6], grid[i,7], grid[i,8], grid[i,9], grid[i,10], grid[i,11], grid[i,12], grid[i,13], grid[i,14], grid[i,15]))
}

# Run the loop in parallel
result = foreach(i = 1:nrow(grid), .combine = rbind, .packages = "stats") %dopar% {
  fit_model(grid, i, est.el1)
}
stopCluster(cl)

# Transfer results and rename columns
grid = data.table(result)
colnames(grid) = c("i", "r", "alpha", "s", "beta", "CET_U_diff", "PTS_U_diff", "U_diff", "CET_L_diff", "PTS_L_diff", "L_diff", "CET_t_Udiff", "CET_t_Ldiff", "PTS_t_Udiff", "PTS_t_Ldiff")


# # Assign column names and convert to data frame
# colnames(results) <- c("param1", "param2", "outcome")
# results_df <- as.data.frame(results)

# Evaluation (selected, takes the formula approach, not the direct approach)
# Get the intervals

index_list = list()

# Upper CET
min(abs(grid$CET_t_Udiff), na.rm = TRUE)
grid[CET_t_Udiff <= min(abs(grid$CET_t_Udiff), na.rm = TRUE)]
i = as.numeric(grid[CET_t_Udiff <= min(abs(grid$CET_t_Udiff), na.rm = TRUE)][1,1])
index_list = append(index_list, i)
est.el1@prediction.params.model[1] = as.numeric(grid[i,2])
est.el1@prediction.params.model[2] = as.numeric(grid[i,3])
est.el1@prediction.params.model[3] = as.numeric(grid[i,4])
est.el1@prediction.params.model[4] = as.numeric(grid[i,5])

if (end1 > 0){
  pred = predict(est.el1, predict.spending = TRUE, prediction.end = end1)
}else{
  pred = predict(est.el1, predict.spending = TRUE)
}

pred
sum(pred$actual.x >= pred$CET)/nrow(pred)
CET_upper = pred$CET

# Lower CET
min(abs(grid$CET_t_Ldiff), na.rm = TRUE)
grid[CET_t_Ldiff <= min(abs(grid$CET_t_Ldiff), na.rm = TRUE)]
i = as.numeric(grid[CET_t_Ldiff <= min(abs(grid$CET_t_Ldiff), na.rm = TRUE)][1,1])
index_list = append(index_list, i)
est.el1@prediction.params.model[1] = as.numeric(grid[i,2])
est.el1@prediction.params.model[2] = as.numeric(grid[i,3])
est.el1@prediction.params.model[3] = as.numeric(grid[i,4])
est.el1@prediction.params.model[4] = as.numeric(grid[i,5])

if (end1 > 0){
  pred = predict(est.el1, predict.spending = TRUE, prediction.end = end1)
}else{
  pred = predict(est.el1, predict.spending = TRUE)
}

sum(pred$actual.x + CET_tolerance <= pred$CET)/nrow(pred)
pred$ok = pred$actual.x + CET_tolerance <= pred$CET
CET_lower = pred$CET

# Upper PTS
min(abs(grid$PTS_t_Udiff), na.rm = TRUE)
grid[PTS_U_diff <= min(abs(grid$PTS_t_Udiff), na.rm = TRUE)]
i = as.numeric(grid[PTS_U_diff <= min(abs(grid$PTS_t_Udiff), na.rm = TRUE)][1,1])
index_list = append(index_list, i)
est.el1@prediction.params.model[1] = as.numeric(grid[i,2])
est.el1@prediction.params.model[2] = as.numeric(grid[i,3])
est.el1@prediction.params.model[3] = as.numeric(grid[i,4])
est.el1@prediction.params.model[4] = as.numeric(grid[i,5])

if (end1 > 0){
  pred = predict(est.el1, predict.spending = TRUE, prediction.end = end1)
}else{
  pred = predict(est.el1, predict.spending = TRUE)
}

sum(pred$actual.total.spending >= pred$predicted.total.spending)/nrow(pred)
PTS_upper = pred$predicted.total.spending

# Lower PTS
min(abs(grid$PTS_t_Ldiff), na.rm = TRUE)
grid[PTS_t_Ldiff <= min(abs(grid$PTS_t_Ldiff), na.rm = TRUE)]
i = as.numeric(grid[PTS_t_Ldiff <= min(abs(grid$PTS_t_Ldiff), na.rm = TRUE)][1,1])
index_list = append(index_list, i)
est.el1@prediction.params.model[1] = as.numeric(grid[i,2])
est.el1@prediction.params.model[2] = as.numeric(grid[i,3])
est.el1@prediction.params.model[3] = as.numeric(grid[i,4])
est.el1@prediction.params.model[4] = as.numeric(grid[i,5])

if (end1 > 0){
  pred = predict(est.el1, predict.spending = TRUE, prediction.end = end1)
}else{
  pred = predict(est.el1, predict.spending = TRUE)
}

pred$ok = pred$actual.total.spending + PTS_tolerance <= pred$predicted.total.spending
sum(pred$actual.total.spending + PTS_tolerance <= pred$predicted.total.spending)/nrow(pred)
PTS_lower = pred$predicted.total.spending

intervals_QR = data.table("Id" = pred$Id,
                          "CET_lower" = round(CET_lower, 4),
                          "CET_upper" = CET_upper,
                          "CET_true" = pred$actual.x,
                          "CET_prediction" = results.el1$CET,
                          "CET_covered" = results.el1$actual.x + CET_tolerance > CET_lower & results.el1$actual.x < CET_upper,
                          "PTS_lower" = round(PTS_lower, 4),
                          "PTS_upper" = PTS_upper,
                          "PTS_true" = results.el1$actual.total.spending,
                          "PTS_pred" = results.el1$predicted.total.spending,
                          "PTS_covered" = results.el1$actual.total.spending + PTS_tolerance > PTS_lower & results.el1$actual.total.spending < PTS_upper)

