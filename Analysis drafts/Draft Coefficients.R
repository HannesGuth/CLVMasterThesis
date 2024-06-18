# Modifying parameters

# Create datatables and copy the estimated model
m_CET = data.table("Id" = results_general$Id)
m_PTS = data.table("Id" = results_general$Id)

est.pnbd = pnbd(clv.data = clv.gift2, verbose = TRUE)
summary(est.pnbd)
model_PNDB = copy(est.pnbd)
######

# Create parameter tables by the help of a multivariate normal distribution over the parameter covariance matrix

estpnbd_cov_table = data.table(t(mvrnorm(n = 1, as.numeric(est.pnbd@prediction.params.model), vcov(est.pnbd), tol = 1e-06, empirical = FALSE)))
estpnbd_cov_table = estpnbd_cov_table[-1,]
# estgg_cov_table = data.table(t(mvrnorm(n = 1, as.numeric(est.gg@prediction.params.model), vcov(est.gg), tol = 1e-06, empirical = FALSE)))
# x = data.table(t(mvrnorm(n = 1, as.numeric(est.pnbd@prediction.params.model), vcov(est.pnbd), tol = 1e-06, empirical = FALSE)))
# Create tables containing the parameter estimates that are supposed to follow a normal distribution considering the covariance matrix
while (nrow(estpnbd_cov_table) < 1000){
  row_pnbd = data.table(t(mvrnorm(n = 1, as.numeric(est.pnbd@prediction.params.model), vcov(est.pnbd), tol = 1e-06, empirical = FALSE)))
  # row_x = data.table(t(mvrnorm(n = 1, as.numeric(est.pnbd@prediction.params.model), vcov(est.pnbd), tol = 1e-06, empirical = FALSE)))
  # row_gg = data.table(t(mvrnorm(n = 1, as.numeric(est.gg@prediction.params.model), vcov(est.gg), tol = 1e-06, empirical = FALSE)))
  # x = rbind(x, row_x)
  # Ruling out the cases where parameters are estimated < 0 what would result in errors (unfortunately, by this, one introduces bias)
  
  # if (all(row_pnbd > 0)){
  #   estpnbd_cov_table = rbind(estpnbd_cov_table, row_pnbd)
  # }
  
  row_pnbd = (row_pnbd > 0) * row_pnbd + (row_pnbd < 0) * 0 # rectified normal distribution, see 57PI, p.8
  estpnbd_cov_table = rbind(estpnbd_cov_table, row_pnbd)
  # if (all(row_gg > 0)){
  #   estgg_cov_table = rbind(estgg_cov_table, row_gg)
  # }
}
hist(estpnbd_cov_table$r)
hist(estpnbd_cov_table$alpha)
hist(estpnbd_cov_table$s)
hist(estpnbd_cov_table$beta)

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
  hist(unlist(m_CET[i,2:1000]), breaks = 40)
  hist(unlist(m_PTS[i,2:1000]), breaks = 40)
}

# Summarize the data in a datatable
intervals_PB = data.table("Id" = pred_PNBD$Id,
                          "CET_lower" = 0,
                          "CET_upper" = 0,
                          "CET_true" = results_general$actual.x,
                          "CET_prediction" = results_general$CET,
                          "CET_covered" = 0,
                          "PTS_lower" = 0,
                          "PTS_upper" = 0,
                          "PTS_true" = results_general$actual.total.spending,
                          "PTS_prediction" = results_general$predicted.total.spending,
                          "PTS_covered" = 0
)

# Get the intervals
for (i in 1:nrow(pred_PNBD)){
  intervals_PB[i,2] = quantile(unlist(m_CET[i,2:1000]), probs = c(0.05,0.95), na.rm = TRUE)[1]
  intervals_PB[i,3] = quantile(unlist(m_CET[i,2:1000]), probs = c(0.05,0.95), na.rm = TRUE)[2]
  intervals_PB[i,7] = quantile(unlist(m_PTS[i,2:1000]), probs = c(0.05,0.95), na.rm = TRUE)[1]
  intervals_PB[i,8] = quantile(unlist(m_PTS[i,2:1000]), probs = c(0.05,0.95), na.rm = TRUE)[2]
}

intervals_PB$CET_covered = intervals_PB$CET_lower <= intervals_PB$CET_true & intervals_PB$CET_true <= intervals_PB$CET_upper
intervals_PB$PTS_covered = intervals_PB$PTS_lower <= intervals_PB$PTS_true & intervals_PB$PTS_true <= intervals_PB$PTS_upper

# Performance Ensembles
mean(intervals_PB$CET_covered)
mean(intervals_PB$PTS_covered)
