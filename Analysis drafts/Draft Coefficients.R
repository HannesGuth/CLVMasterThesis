# install.packages("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/CLVTools", repos = NULL, type="source")
# 
# # Load package
# library(CLVTools)
# library(data.table)
# library(compiler)
# library(ggplot2)
# library(profvis)
# library(rockchalk)
# 
# # Load data
# data("apparelTrans")
# clv.apparel <- clvdata(apparelTrans,  
#                        date.format="ymd", 
#                        time.unit = "week",
#                        estimation.split = 40,
#                        name.id = "Id",
#                        name.date = "Date",
#                        name.price = "Price")
# 
# # Estimate standard Pareto/NBD Model
# est.pnbd <- pnbd(clv.data = clv.apparel, verbose = TRUE)
# summary(est.pnbd)
# results <- predict(est.pnbd, predict.spending = TRUE)
# print(results)
# est.gg <- gg(clv.data = clv.apparel)
# predict(est.gg)
# 
# # Boostrapping to get prediction intervalls
# set.seed(1)
# results_boots <- predict(est.pnbd, uncertainty="boots")
# 
# # A more detailed look at the results
# results_boots$predicted.CLV
# results_boots$predicted.CLV.CI.5
# results_boots$predicted.CLV.CI.95
# results_boots$actual.total.spending
# hist((results_boots$predicted.CLV-results_boots$predicted.CLV.CI.5)/results_boots$predicted.CLV*100)
# hist((results_boots$predicted.CLV.CI.95-results_boots$predicted.CLV)/results_boots$predicted.CLV*100)

# Modifying parameters

# Create datatables and copy the estimated model
m_CET = data.table("Id" = results_boots$Id)
m_PTS = data.table("Id" = results_boots$Id)

est.pnbd = pnbd(clv.data = clv.apparel, verbose = TRUE)
model_PNDB = copy(est.pnbd)

######

# Create parameter tables by the help of a multivariate normal distribution over the parameter covariance matrix

estpnbd_cov_table = data.table(t(mvrnorm(n = 1, as.numeric(est.pnbd@prediction.params.model), vcov(est.pnbd), tol = 1e-06, empirical = FALSE)))
estgg_cov_table = data.table(t(mvrnorm(n = 1, as.numeric(est.gg@prediction.params.model), vcov(est.gg), tol = 1e-06, empirical = FALSE)))
# x = data.table(t(mvrnorm(n = 1, as.numeric(est.pnbd@prediction.params.model), vcov(est.pnbd), tol = 1e-06, empirical = FALSE)))
# Create tables containing the parameter estimates that are supposed to follow a normal distribution considering the covariance matrix
while (nrow(estpnbd_cov_table) < 1000){
  row_pnbd = data.table(t(mvrnorm(n = 1, as.numeric(est.pnbd@prediction.params.model), vcov(est.pnbd), tol = 1e-06, empirical = FALSE)))
  # row_x = data.table(t(mvrnorm(n = 1, as.numeric(est.pnbd@prediction.params.model), vcov(est.pnbd), tol = 1e-06, empirical = FALSE)))
  # row_gg = data.table(t(mvrnorm(n = 1, as.numeric(est.gg@prediction.params.model), vcov(est.gg), tol = 1e-06, empirical = FALSE)))
  # x = rbind(x, row_x)
  # Ruling out the cases where parameters are estimated < 0 what would result in errors (unfortunately, by this, one introduces bias)
  if (all(row_pnbd > 0)){
    estpnbd_cov_table = rbind(estpnbd_cov_table, row_pnbd)
  }
  # if (all(row_gg > 0)){
  #   estgg_cov_table = rbind(estgg_cov_table, row_gg)
  # }
}


# Build models and estimate CLVs for all the above created parameter realizations
for (i in 1:1000){
  print(i)
  
  est.pnbd@prediction.params.model[1] = as.numeric(estpnbd_cov_table[i,1])
  est.pnbd@prediction.params.model[2] = as.numeric(estpnbd_cov_table[i,2])
  est.pnbd@prediction.params.model[3] = as.numeric(estpnbd_cov_table[i,3])
  est.pnbd@prediction.params.model[4] = as.numeric(estpnbd_cov_table[i,4])
  
  pred_PNBD = predict(est.pnbd)
  
  column = toString(i)
  m_CET[, (column) := pred_PNBD$CET]
  m_PTS[, (column) := pred_PNBD$predicted.total.spending]
}

# Example histograms of the CLV distribution for the first 20 customers
for (i in 1:20){
  hist(unlist(m_CET[i,2:1000]), breaks = 200)
  hist(unlist(m_PTS[i,2:1000]), breaks = 200)
}

# Summarize the data in a datatable
intervals_PB = data.table("Id" = results_boots$Id,
                          "Mod_CET" = results_boots$CET,
                          "Mod_CET05" = results_boots$CET.CI.5,
                          "Mod_CET95" = results_boots$CET.CI.95,
                          "Mod_PTS" = results_boots$predicted.total.spending,
                          "Mod_PTS05" = 0, #results_boots$predicted.total.spending.CI.5,
                          "Mod_PTS95" = 0, #results_boots$predicted.total.spending.CI.95,
                          "PB_CET_05" = 0,
                          "PB_CET_95" = 0,
                          "PB_PTS_05" = 0,
                          "PB_PTS_95" = 0,
                          "CET_true" = results$actual.x,
                          "PTS_true" = results$actual.total.spending
)

# Get the intervals
for (i in 1:250){
  intervals_PB[i,8] = quantile(unlist(m_CET[i,2:1000]), probs = c(0.05,0.95), na.rm = TRUE)[1]
  intervals_PB[i,9] = quantile(unlist(m_CET[i,2:1000]), probs = c(0.05,0.95), na.rm = TRUE)[2]
  intervals_PB[i,10] = quantile(unlist(m_PTS[i,2:1000]), probs = c(0.05,0.95), na.rm = TRUE)[1]
  intervals_PB[i,11] = quantile(unlist(m_PTS[i,2:1000]), probs = c(0.05,0.95), na.rm = TRUE)[2]
}

# Performance Ensembles
sum(intervals_PB$PB_CET_05 < intervals_PB$CET_true & intervals_PB$CET_true < intervals_PB$PB_CET_95)/nrow(results)
sum(intervals_PB$PB_PTS_05 < intervals_PB$PTS_true & intervals_PB$PTS_true < intervals_PB$PB_PTS_95)/nrow(results)

# Performance Bootstrap
sum(intervals_PB$CET_true > intervals_PB$Mod_CET05 & intervals_PB$CET_true < intervals_PB$Mod_CET95)/nrow(results)

# Check for quality of the intervals
# intervals_PB$PBlower = intervals$`PB_CLV_05%`/intervals$CLV
# intervals_PB$PBupper = intervals$`PB_CLV_95%`/intervals$CLV
# intervals_PB$modlower = intervals$CLV_05/intervals$CLV
# intervals_PB$modupper = intervals$CLV_95/intervals$CLV
# 
# sum(intervals_PB$`PB_CLV_05%` < intervals_PB$CLV_05 & intervals_PB$`PB_CLV_95%` < intervals_PB$CLV_95)
# sum(intervals_PB$`PB_CLV_05%` > intervals_PB$CLV_05 & intervals_PB$`PB_CLV_95%` > intervals_PB$CLV_95)
# sum(intervals_PB$`PB_CLV_95%` > intervals_PB$CLV_95)
# 
# mean(intervals_PB$PBlower)
# mean(intervals_PB$modlower)
# mean(intervals_PB$PBupper)
# mean(intervals_PB$modupper)