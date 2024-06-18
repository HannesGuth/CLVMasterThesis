library(BTYD)
library(BTYDplus)


########################################################################################## For gift1
library(CLVTools)
library(BTYDplus)
# data("apparelTrans")
datecustomer = gift2[,c(1,2)]
colnames(datecustomer) = c("cust", "date")

# Convert data into BTYD format
gift2cbs = elog2cbs(datecustomer, T.cal = min(datecustomer$date) + 7*splitweek)
params.pnbd = BTYD::pnbd.EstimateParameters(gift2cbs[, c("x", "t.x", "T.cal")])
BTYD::pnbd.cbs.LL(params.pnbd, gift2cbs[, c("x", "t.x", "T.cal")])

# Calculate CET
gift2cbs$xstar.pnbd = BTYD::pnbd.ConditionalExpectedTransactions(
  params = params.pnbd, T.star = 39.85714, # 39.85714 = as.numeric(difftime(as.Date("2005-01-03") + 40*7, max(apparelTrans$Date))) / 7
  x = gift2cbs$x, t.x = gift2cbs$t.x,
  T.cal = gift2cbs$T.cal)

# Draw posterior parameter distribution and posterior predictive distribution
pnbd.draws = pnbd.mcmc.DrawParameters(gift2cbs) # gives an error
pnbd.xstar.draws = mcmc.DrawFutureTransactions(gift2cbs, pnbd.draws)

# Collect the data
intervals_BA = data.table("Id" = pred_PNBD$Id,
                          "CET_lower" = 0,
                          "CET_upper" = 0,
                          "CET_true" = results_general$actual.x,
                          "CET_prediction" = 0,
                          "CET_covered" = 0,
                          "PTS_lower" = NA,
                          "PTS_upper" = NA,
                          "PTS_true" = NA,
                          "PTS_prediction" = NA,
                          "PTS_covered" = NA
)

# Take the intervals of CET
for (i in 1:nrow(pred_PNBD)){
  intervals_BA[i,2] = quantile(pnbd.xstar.draws[,i], probs = 0.05)
  intervals_BA[i,3] = quantile(pnbd.xstar.draws[,i], probs = 0.95)
  #intervals_BA[i,5] = quantile(pnbd.xstar.draws[,i], probs = 0.50) # assume the median as prediction
  intervals_BA[i,5] = mean(pnbd.xstar.draws[,i])
  print(mean(pnbd.xstar.draws[,i]))
}

intervals_BA$CET_covered = intervals_BA$CET_true >= intervals_BA$CET_lower & intervals_BA$CET_true <= intervals_BA$CET_upper

# Performance measure
mean(intervals_BA$CET_covered)


