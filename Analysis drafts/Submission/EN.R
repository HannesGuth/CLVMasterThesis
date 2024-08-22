# Modifying parameters

# Create datatables and copy the estimated model
m_CET = data.table("Id" = results_general$Id)
m_PTS = data.table("Id" = results_general$Id)
n = 100

est.pnbd = pnbd(clv.data = clv.data2, verbose = TRUE)
######

# Create parameter tables by the help of a multivariate normal distribution over the parameter covariance matrix

estpnbd_cov_table = data.table(t(mvrnorm(n = 1, as.numeric(est.pnbd@prediction.params.model), vcov(est.pnbd), tol = 1e-06, empirical = FALSE)))
estpnbd_cov_table = estpnbd_cov_table[-1,]

# Create tables containing the parameter estimates that are supposed to follow a normal distribution considering the covariance matrix
while (nrow(estpnbd_cov_table) < n){
  row_pnbd = data.table(t(mvrnorm(n = 1, as.numeric(est.pnbd@prediction.params.model), vcov(est.pnbd), tol = 1e-06, empirical = FALSE)))
  
  row_pnbd = (row_pnbd > 0) * row_pnbd + (row_pnbd < 0) * 0 # rectified normal distribution, see 57PI, p.8
  estpnbd_cov_table = rbind(estpnbd_cov_table, row_pnbd)
}

# Build models and estimate CLVs for all the above created parameter realizations
for (i in 1:n){
  print(i)
  
  est.pnbd@prediction.params.model[1] = as.numeric(estpnbd_cov_table[i,1])
  est.pnbd@prediction.params.model[2] = as.numeric(estpnbd_cov_table[i,2])
  est.pnbd@prediction.params.model[3] = as.numeric(estpnbd_cov_table[i,3])
  est.pnbd@prediction.params.model[4] = as.numeric(estpnbd_cov_table[i,4])
  
  if (whole_period2){
    pred_PNBD = predict(est.pnbd)
  }else{
    pred_PNBD = predict(est.pnbd, prediction.end = end2)
  }
  
  
  column = toString(i)
  m_CET[, (column) := pred_PNBD$CET]
  m_PTS[, (column) := pred_PNBD$predicted.total.spending]
}

# Summarize the data in a datatable
intervals_EN = data.table("Id" = results_general$Id,
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
  intervals_EN[i,2] = quantile(unlist(m_CET[i,2:n]), probs = c(alpha/2,1-(alpha/2)), na.rm = TRUE)[1]
  intervals_EN[i,3] = quantile(unlist(m_CET[i,2:n]), probs = c(alpha/2,1-(alpha/2)), na.rm = TRUE)[2]
  intervals_EN[i,7] = quantile(unlist(m_PTS[i,2:n]), probs = c(alpha/2,1-(alpha/2)), na.rm = TRUE)[1]
  intervals_EN[i,8] = quantile(unlist(m_PTS[i,2:n]), probs = c(alpha/2,1-(alpha/2)), na.rm = TRUE)[2]
}

intervals_EN$CET_covered = intervals_EN$CET_lower <= intervals_EN$CET_true & intervals_EN$CET_true <= intervals_EN$CET_upper
intervals_EN$PTS_covered = intervals_EN$PTS_lower <= intervals_EN$PTS_true & intervals_EN$PTS_true <= intervals_EN$PTS_upper

# Performance Ensembles
mean(intervals_EN$CET_covered)
mean(intervals_EN$PTS_covered)
