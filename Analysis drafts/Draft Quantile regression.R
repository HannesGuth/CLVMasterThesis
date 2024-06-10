# library(optimx)
# library(compiler)
# library(ggplot2)
# library(profvis)
# library(rockchalk)
# library(TMB)
# data("apparelTrans")
# remotes::install_github("kaskr/adcomp/TMB")

# r = 0.7858 # parameter
# x = 2 # already made transactions in the calibration period
# a = 5.3312 # parameter
# b = 11.8221 # parameter
# s = 0.3606 # parameter
# t = 26 # point in time of the last transaction
# TT = 40 # current point in time

# F1 = 
# L = ((gamma(r+x)*(a^r)*b^s) / gamma(r)) * ((s/(r+s+x)) * )
# counter = 0
est = pnbd(clv.data = clv.apparel, verbose = TRUE)


# Optim package (not selected)
for (i in 1:1){
  # objective = function(params){
  #   counter = counter + 1
  #   est@prediction.params.model[1] = params[1]
  #   est@prediction.params.model[2] = params[2]
  #   est@prediction.params.model[3] = params[3]
  #   est@prediction.params.model[4] = params[4]
  #   results_opt = predict(est, predict.spending = TRUE)
  #   Sys.sleep(0.1)
  #   diff = sum(results_opt$actual.x - results_opt$CET)
  #   rm(results_opt)
  #   print(diff)
  #   print(params)
  #   print(paste("counter: ", counter))
  #   return(diff)
  # }
  # 
  # initial_params = rep(0.5,4)
  # lower_bounds = c(0, 0, 0, 0)
  # upper_bounds = c(10, 10, 10, 10)
  # 
  # result <- optimx(
  #   par = initial_params,
  #   fn = objective,
  #   method = "L-BFGS-B",
  #   control = list(maxit = 30000)
  #   # lower = lower_bounds,
  #   # upper = upper_bounds,
  #   # control = list(fnscale = -1) # optional, for maximization problems
  # )
  # result
}

# Grid (not selected)
for (i in 1:1){
  # grid = expand.grid(seq(0.2,0.8,0.1), seq(0.5,10,1.5), seq(0.2,0.8,0.1), seq(0.5,15,1.5))
  # alpha = 0.1
  # best_CET_i = 0
  # best_PTS_i = 0
  # CET_diffsum = Inf
  # PTS_diffsum = Inf
  # best_CET_diff = Inf
  # best_PTS_diff = Inf
  # 
  # for (i in 1:nrow(grid)){
  #   print(i)
  #   print(paste("best_CET_diff: ", best_CET_diff))
  #   est@prediction.params.model[1] = grid[i,1]
  #   est@prediction.params.model[2] = grid[i,2]
  #   est@prediction.params.model[3] = grid[i,3]
  #   est@prediction.params.model[4] = grid[i,4]
  #   pred = predict(est, predict.spending = TRUE)
  #   CET_diff = max(alpha * (sum(pred$actual.x - pred$CET)/nrow(pred)),
  #                  (1-alpha) * (sum(pred$actual.x - pred$CET)/nrow(pred)))
  #   if (CET_diff < best_CET_diff){
  #     best_CET_diff = CET_diff
  #     best_CET_i = i
  #   }
  #   PTS_diff = max(alpha * (sum(pred$actual.total.spending - pred$predicted.total.spending)/nrow(pred)),
  #                  (1-alpha) * (sum(pred$actual.total.spending - pred$predicted.total.spending)/nrow(pred)))
  #   if (PTS_diff < best_PTS_diff){
  #     best_PTS_diff = PTS_diff
  #     best_PTS_i = i
  #   }
  # }
}

# Attempt to recalculate the model outputs to insert it in the optimization instead of the reals model
# L = function(r,a,s,b,x,tx,TT){
#   ((gamma(r+x)*(a^r)*(b^s))/gamma(r)) * ((s/(r+s+x))*Re(hypergeo(r+s+x, (a>b)*(s+1) + (a<b)*(r+x), r+s+x+1, abs(b-a)/(max(a,b)+t)))/((b+t)^(r+s+x)) + ((r+x)/(r+s+x)) * Re(hypergeo(r+s+x, (a>b)*s + (a<b)*(r+x+1), r+s+x+1, abs(b-a)/(max(a,b)+TT)))/((max(a,b)+TT)^(r+s+x))) 
# }
# E = function(r,a,s,b,x,t,TT){
#   (((gamma(r+x)*(a^r)*(b^s))/(gamma(r)*((a+TT)^(r+x))*(b+TT)^s)) / L(r,a,s,b,x,t,TT)) * (((r+x)*(b+TT))/((a+TT)*(s-1))) * (1-(((b+TT)/(b+TT+t))^(s-1)))
# }
# E(r,a,s,b,x,t,TT)


###################################################################

# Grid (not selected)
for (i in 1:1){
  # grid = expand.grid(seq(14,15,1), seq(0.1,5,3), c(seq(0,0.5,0.5), seq(7,11,2)), seq(1,10,3))
  # grid = cbind(i = seq(1, nrow(grid)), grid)
  # grid$maxCET = 0
  # grid$maxPTS = 0
  # grid$maxDiff = 0
  # grid$minCET = 0
  # grid$minPTS = 0
  # grid$minDiff = 0
  # 
  # alpha = 0.05
  # CET_tolerance = 0.1
  # PTS_tolerance = 1.5
  # best_CET_i = 0
  # best_PTS_i = 0
  # CET_diffsummax = Inf
  # PTS_diffsummax = Inf
  # CET_diffsummin = Inf
  # PTS_diffsummin = Inf
  # best_CET_diff = Inf
  # best_PTS_diff = Inf
  # 
  # # Set up the parallel backend
  # num_cores <- parallel::detectCores()
  # cl <- makeCluster(num_cores - 2)
  # registerDoParallel(cl)
  # 
  # # Define the model fitting function
  # fit_model <- function(grid, i, est) {
  #   print(i)
  #   est@prediction.params.model[1] = grid[i,2]
  #   est@prediction.params.model[2] = grid[i,3]
  #   est@prediction.params.model[3] = grid[i,4]
  #   est@prediction.params.model[4] = grid[i,5]
  #   pred = predict(est, predict.spending = TRUE)
  #   
  #   # Upper
  #   CET_diffsummax = (sum(pred$actual.x >= pred$CET)/nrow(pred)) - alpha
  #   PTS_diffsummax = (sum(pred$actual.total.spending >= pred$predicted.total.spending)/nrow(pred)) - alpha
  #   
  #   # Lower
  #   CET_diffsummin = (sum(pred$actual.x + CET_tolerance <= pred$CET)/nrow(pred)) - alpha
  #   PTS_diffsummin = (sum(pred$actual.total.spending + PTS_tolerance <= pred$predicted.total.spending)/nrow(pred)) - alpha
  #   
  #   grid[i,6] = CET_diffsummax
  #   grid[i,7] = PTS_diffsummax
  #   grid[i,8] = abs(CET_diffsummax) + abs(PTS_diffsummax)
  #   
  #   grid[i,9] = CET_diffsummin
  #   grid[i,10] = PTS_diffsummin
  #   grid[i,11] = abs(CET_diffsummin) + abs(PTS_diffsummin)
  #   
  #   return(c(grid[i,1], grid[i,2], grid[i,3], grid[i,4], grid[i,5], grid[i,6], grid[i,7], grid[i,8], grid[i,9], grid[i,10], grid[i,11]))
  # }
  # 
  # # Run the loop in parallel
  # result = foreach(i = 1:nrow(grid), .combine = rbind, .packages = "stats") %dopar% {
  #   fit_model(grid, i, est)
  # }
  # stopCluster(cl)
  # 
  # grid = data.table(result)
  # colnames(grid) = c("i", "r", "alpha", "s", "beta", "CET_U_diff", "PTS_U_diff", "U_diff", "CET_L_diff", "PTS_L_diff", "L_diff")
}
# # # # # # #

# Create a grid where parameters are expected to be to achieve the desired quantiles (pre-knowledge helpful to reduce computational effort)
# grid = expand.grid(seq(14,15,1), seq(0.1,5,3), c(seq(0,0.5,0.5), seq(7,11,2)), seq(1,10,3))
grid = expand.grid(seq(1,15,3), seq(0.01,5,2), c(seq(0,0.5,0.3), seq(7,11,2)), seq(1,10,3))
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
fit_model <- function(grid, i, est) {
  print(i)
  est@prediction.params.model[1] = as.numeric(grid[i,2])
  est@prediction.params.model[2] = as.numeric(grid[i,3])
  est@prediction.params.model[3] = as.numeric(grid[i,4])
  est@prediction.params.model[4] = as.numeric(grid[i,5])
  pred = predict(est, predict.spending = TRUE)
  
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
  fit_model(grid, i, est)
}
stopCluster(cl)

# Transfer results and rename columns
grid = data.table(result)
colnames(grid) = c("i", "r", "alpha", "s", "beta", "CET_U_diff", "PTS_U_diff", "U_diff", "CET_L_diff", "PTS_L_diff", "L_diff", "CET_t_Udiff", "CET_t_Ldiff", "PTS_t_Udiff", "PTS_t_Ldiff")


# # Assign column names and convert to data frame
# colnames(results) <- c("param1", "param2", "outcome")
# results_df <- as.data.frame(results)

# Evaluation (not selected, takes old technique (Direct interval estimation))
for (i in 1:1){
# # Upper CET 7, 0.3, 0, 5
# min(abs(grid$CET_U_diff), na.rm = TRUE)
# grid[CET_U_diff <= min(abs(grid$CET_U_diff), na.rm = TRUE)]
# i = as.numeric(grid[CET_U_diff <= min(abs(grid$CET_U_diff), na.rm = TRUE)][1,1])
# est@prediction.params.model[1] = as.numeric(grid[i,2]) # 1.65
# est@prediction.params.model[2] = as.numeric(grid[i,3]) # 0.9
# est@prediction.params.model[3] = as.numeric(grid[i,4]) # 0.001
# est@prediction.params.model[4] = as.numeric(grid[i,5]) # 5
# pred = predict(est, predict.spending = TRUE)
# sum(pred$actual.x >= pred$CET)/nrow(pred)
# CET_upper = pred$CET
# 
# #Lower CET
# min(abs(grid$CET_L_diff), na.rm = TRUE)
# grid[CET_L_diff <= min(abs(grid$CET_L_diff), na.rm = TRUE)]
# i = as.numeric(grid[CET_L_diff <= min(abs(grid$CET_L_diff), na.rm = TRUE)][1,1])
# est@prediction.params.model[1] = as.numeric(grid[i,2]) # 1.65
# est@prediction.params.model[2] = as.numeric(grid[i,3]) # 0.9
# est@prediction.params.model[3] = as.numeric(grid[i,4]) # 0.001
# est@prediction.params.model[4] = as.numeric(grid[i,5]) # 5
# pred = predict(est, predict.spending = TRUE)
# sum(pred$actual.x + CET_tolerance <= pred$CET)/nrow(pred)
# CET_lower = pred$CET
# 
# # Upper PTS
# min(abs(grid$PTS_U_diff), na.rm = TRUE)
# grid[PTS_U_diff <= min(abs(grid$PTS_U_diff), na.rm = TRUE)]
# i = as.numeric(grid[PTS_U_diff <= min(abs(grid$PTS_U_diff), na.rm = TRUE)][1,1])
# est@prediction.params.model[1] = 1#as.numeric(grid[i,2]) # 1.65
# est@prediction.params.model[2] = 4.01#as.numeric(grid[i,3]) # 0.9
# est@prediction.params.model[3] = 11#as.numeric(grid[i,4]) # 0.001
# est@prediction.params.model[4] = 10#as.numeric(grid[i,5]) # 5
# pred = predict(est, predict.spending = TRUE)
# (sum((pred$actual.x < pred$CET) * (1 - alpha) + (pred$actual.x >= pred$CET) * alpha)/nrow(pred) - alpha)
# (sum(pred$actual.total.spending >= pred$predicted.total.spending)/nrow(pred))
# PTS_upper = pred$predicted.total.spending
# 
# #Lower PTS
# min(abs(grid$PTS_L_diff), na.rm = TRUE)
# grid[PTS_L_diff <= min(abs(grid$PTS_L_diff), na.rm = TRUE)]
# i = as.numeric(grid[PTS_L_diff <= min(abs(grid$PTS_L_diff), na.rm = TRUE)][1,1])
# est@prediction.params.model[1] = as.numeric(grid[i,2]) # 1.65
# est@prediction.params.model[2] = as.numeric(grid[i,3]) # 0.9
# est@prediction.params.model[3] = as.numeric(grid[i,4]) # 0.001
# est@prediction.params.model[4] = as.numeric(grid[i,5]) # 5
# pred = predict(est, predict.spending = TRUE)
# sum(pred$actual.total.spending + PTS_tolerance <= pred$predicted.total.spending)/nrow(pred)
# PTS_lower = pred$predicted.total.spending
}

# Evaluation (selected, takes the formula approach, not the direct approach)
# Get the intervals

# Upper CET
min(abs(grid$CET_t_Udiff), na.rm = TRUE)
grid[CET_U_diff <= min(abs(grid$CET_t_Udiff), na.rm = TRUE)]
i = as.numeric(grid[CET_U_diff <= min(abs(grid$CET_t_Udiff), na.rm = TRUE)][1,1])
est@prediction.params.model[1] = as.numeric(grid[i,2]) # 1.65
est@prediction.params.model[2] = as.numeric(grid[i,3]) # 0.9
est@prediction.params.model[3] = as.numeric(grid[i,4]) # 0.001
est@prediction.params.model[4] = as.numeric(grid[i,5]) # 5
pred = predict(est, predict.spending = TRUE)
sum(pred$actual.x >= pred$CET)/nrow(pred)
CET_upper = pred$CET

# Lower CET
min(abs(grid$CET_t_Ldiff), na.rm = TRUE)
grid[CET_L_diff <= min(abs(grid$CET_t_Ldiff), na.rm = TRUE)]
i = as.numeric(grid[CET_L_diff <= min(abs(grid$CET_t_Ldiff), na.rm = TRUE)][1,1])
est@prediction.params.model[1] = as.numeric(grid[i,2]) # 1.65
est@prediction.params.model[2] = as.numeric(grid[i,3]) # 0.9
est@prediction.params.model[3] = as.numeric(grid[i,4]) # 0.001
est@prediction.params.model[4] = as.numeric(grid[i,5]) # 5
pred = predict(est, predict.spending = TRUE)
sum(pred$actual.x + CET_tolerance <= pred$CET)/nrow(pred)
CET_lower = pred$CET

# Upper PTS
min(abs(grid$PTS_t_Udiff), na.rm = TRUE)
grid[PTS_U_diff <= min(abs(grid$PTS_t_Udiff), na.rm = TRUE)]
i = as.numeric(grid[PTS_U_diff <= min(abs(grid$PTS_t_Udiff), na.rm = TRUE)][1,1])
est@prediction.params.model[1] = as.numeric(grid[i,2]) # 1.65
est@prediction.params.model[2] = as.numeric(grid[i,3]) # 0.9
est@prediction.params.model[3] = as.numeric(grid[i,4]) # 0.001
est@prediction.params.model[4] = as.numeric(grid[i,5]) # 5
pred = predict(est, predict.spending = TRUE)
(sum((pred$actual.x < pred$CET) * (1 - alpha) + (pred$actual.x >= pred$CET) * alpha)/nrow(pred) - alpha)
(sum(pred$actual.total.spending >= pred$predicted.total.spending)/nrow(pred))
PTS_upper = pred$predicted.total.spending

# Lower PTS
min(abs(grid$PTS_t_Ldiff), na.rm = TRUE)
grid[PTS_L_diff <= min(abs(grid$PTS_t_Ldiff), na.rm = TRUE)]
i = as.numeric(grid[PTS_L_diff <= min(abs(grid$PTS_t_Ldiff), na.rm = TRUE)][1,1])
est@prediction.params.model[1] = as.numeric(grid[i,2]) # 1.65
est@prediction.params.model[2] = as.numeric(grid[i,3]) # 0.9
est@prediction.params.model[3] = as.numeric(grid[i,4]) # 0.001
est@prediction.params.model[4] = as.numeric(grid[i,5]) # 5
pred = predict(est, predict.spending = TRUE)
sum(pred$actual.total.spending + PTS_tolerance <= pred$predicted.total.spending)/nrow(pred)
PTS_lower = pred$predicted.total.spending

intervals_QR = data.table("Id" = results_boots$Id,
                          "CET_true" = results$actual.x,
                          "CET_pred" = results$CET,
                          "CET_lower" = round(CET_lower, 4),
                          "CET_upper" = CET_upper,
                          "PTS_true" = results$actual.total.spending,
                          "PTS_pred" = results$predicted.total.spending,
                          "PTS_lower" = round(PTS_lower, 4),
                          "PTS_upper" = PTS_upper)

sum(intervals_QR$CET_true + CET_tolerance > intervals_QR$CET_lower & intervals_QR$CET_true < intervals_QR$CET_upper)/nrow(results)
sum(intervals_QR$PTS_true + PTS_tolerance > intervals_QR$PTS_lower & intervals_QR$PTS_true < intervals_QR$PTS_upper)/nrow(results)
sum(intervals_PB$CET_true > intervals_PB$Mod_CET05 & intervals_PB$CET_true < intervals_PB$Mod_CET95)/nrow(results)


