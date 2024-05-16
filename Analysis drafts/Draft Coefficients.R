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
m_DERT = data.table("Id" = results_boots$Id)
m_PMS = data.table("Id" = results_boots$Id)
m_CLV = data.table("Id" = results_boots$Id)

est.pnbd <- pnbd(clv.data = clv.apparel, verbose = TRUE)
est.gg <- gg(clv.data = clv.apparel)
model_PNDB = copy(est.pnbd)
model_GG = copy(est.gg)

######

# 
estpnbd_cov_table = data.table(t(mvrnorm(n = 1, as.numeric(est.pnbd@prediction.params.model), vcov(est.pnbd), tol = 1e-06, empirical = FALSE)))
estgg_cov_table = data.table(t(mvrnorm(n = 1, as.numeric(est.gg@prediction.params.model), vcov(est.gg), tol = 1e-06, empirical = FALSE)))

# Create tables containing the parameter estimates that are supposed to follow a normal distribution considering the covariance matrix
while (nrow(estpnbd_cov_table) < 1000){
  row_pnbd = data.table(t(mvrnorm(n = 1, as.numeric(est.pnbd@prediction.params.model), vcov(est.pnbd), tol = 1e-06, empirical = FALSE)))
  row_gg = data.table(t(mvrnorm(n = 1, as.numeric(est.gg@prediction.params.model), vcov(est.gg), tol = 1e-06, empirical = FALSE)))
  # Ruling out the cases where parameters are estimated < 0 what would result in errors (unfortunately, by this, one introduces bias)
  if (all(row_pnbd > 0)){
    estpnbd_cov_table = rbind(estpnbd_cov_table, row_pnbd)
  }
  if (all(row_gg > 0)){
    estgg_cov_table = rbind(estgg_cov_table, row_gg)
  }
}

# Build models and estimate CLVs for all the above created parameter realizations
for (i in 1:1000){
  print(i)
  
  est.pnbd@prediction.params.model[1] = as.numeric(estpnbd_cov_table[i,1])
  est.pnbd@prediction.params.model[2] = as.numeric(estpnbd_cov_table[i,2])
  est.pnbd@prediction.params.model[3] = as.numeric(estpnbd_cov_table[i,3])
  est.pnbd@prediction.params.model[4] = as.numeric(estpnbd_cov_table[i,4])

  est.gg@prediction.params.model[1] = as.numeric(estgg_cov_table[i,1])
  est.gg@prediction.params.model[2] = as.numeric(estgg_cov_table[i,2])
  est.gg@prediction.params.model[3] = as.numeric(estgg_cov_table[i,3])
  
  pred_PNBD = predict(est.pnbd)
  pred_GG = predict(est.gg)
  
  column = toString(i)
  m_DERT[, (column) := pred_PNBD$DERT]
  m_PMS[, (column) := pred_GG$predicted.mean.spending]
}

# Calculate the CLV with DERT and predicted mean spending
PB_CLV = m_PMS[,2:1000]*m_DERT[,2:1000]

# Example histograms of the CLV distribution for the first 20 customers
for (i in 1:20){
  hist(unlist(PB_CLV[i,]), breaks = 200)
}

# Summarize the data in a datatable
intervals_PB = data.table("Id" = results_boots$Id,
                       "Mod_DERT" = results_boots$DERT,
                       "Mod_DERT05" = results_boots$DERT.CI.5,
                       "Mod_DERT95" = results_boots$DERT.CI.95,
                       "Mod_PMS" = results_boots$predicted.mean.spending,
                       "Mod_PMS05" = results_boots$predicted.mean.spending.CI.5,
                       "Mod_PMS95" = results_boots$predicted.mean.spending.CI.95,
                       "PB_CLV_05%" = 0,
                       "PB_CLV_95%" = 0,
                       "CLV_05" = results_boots$predicted.CLV.CI.5,
                       "CLV_95" = results_boots$predicted.CLV.CI.95,
                       "CLV" = results_boots$predicted.CLV
)

for (i in 1:250){
  intervals_PB[i,8] = quantile(unlist(PB_CLV[i,]), probs = c(0.05,0.95), na.rm = TRUE)[1]
  intervals_PB[i,9] = quantile(unlist(PB_CLV[i,]), probs = c(0.05,0.95), na.rm = TRUE)[2]
}

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