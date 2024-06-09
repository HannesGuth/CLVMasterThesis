library(BTYD)
library(BTYDplus)
# 
# data("groceryElog")
# 
# plotTimingPatterns(groceryElog, n = 30, T.cal = "2007-05-15",
#                    headers = c("Past", "Future"), title = "")
# 
# if (!exists("groceryCBS")) {
#   data("groceryElog")
#   groceryCBS <- elog2cbs(groceryElog, T.cal = "2006-12-31")
# }
# 
# params.pnbd <- BTYD::pnbd.EstimateParameters(groceryCBS[, c("x", "t.x", "T.cal")])
# BTYD::pnbd.cbs.LL(params.pnbd, groceryCBS[, c("x", "t.x", "T.cal")])
# 
# # Conditional Expected Transactions (CET)
# groceryCBS$xstar.pnbd <- BTYD::pnbd.ConditionalExpectedTransactions(
#   params = params.pnbd, T.star = 52,
#   x = groceryCBS$x, t.x = groceryCBS$t.x,
#   T.cal = groceryCBS$T.cal)
# 
# # compare predictions with actuals at aggregated level
# rbind("Actuals" = c("Holdout" = sum(groceryCBS$x.star)),
#        "Pareto/NBD" = c("Holdout" = round(sum(groceryCBS$xstar.pnbd))))
# 
# # P(alive) for customers who’ve had 1 to 5 transactions in first 12 weeks, but then remained inactive for 40 weeks
# palive.pnbd <- BTYD::pnbd.PAlive(params.pnbd,
#                                  x = 1:6, t.x = 12, T.cal = 52)
# 
# if (!exists("groceryCBS")) {
#   data("groceryElog")
#   groceryCBS <- elog2cbs(groceryElog, T.cal = "2006-12-31")
# }
# 
# # generate parameter draws (~13secs on 2015 MacBook Pro)
# pnbd.draws <- pnbd.mcmc.DrawParameters(groceryCBS) # gives an error
# pnbd.xstar.draws <- mcmc.DrawFutureTransactions(groceryCBS, pnbd.draws)
# 
# for (i in 1:20){
#   #plot(density(pnbd.xstar.draws[,i]))
#   print(quantile(pnbd.xstar.draws[,i], probs = c(0.05, 0.95)))
# }
# 
# quantile(pnbd.xstar.draws[,3], probs = c(0.05, 0.95))
# 
# op <- par(mfrow = c(2, 4), mar = c(2.5, 2.5, 2.5, 2.5))
# coda::traceplot(pnbd.draws$level_2)
# coda::densplot(pnbd.draws$level_2)
# par(op)





########################################################################################## For apparelTrans
library(CLVTools)
library(BTYDplus)
data("apparelTrans")
datecustomer = apparelTrans[,c(1,2)]
colnames(datecustomer) = c("cust", "date")

aptcbs = elog2cbs(datecustomer, T.cal = "2005-10-08")
params.pnbd = BTYD::pnbd.EstimateParameters(aptcbs[, c("x", "t.x", "T.cal")])
BTYD::pnbd.cbs.LL(params.pnbd, aptcbs[, c("x", "t.x", "T.cal")])

# Calculate CET
aptcbs$xstar.pnbd = BTYD::pnbd.ConditionalExpectedTransactions(
  params = params.pnbd, T.star = 39.85714, # 39.85714 = as.numeric(difftime(as.Date("2005-01-03") + 40*7, max(apparelTrans$Date))) / 7
  x = aptcbs$x, t.x = aptcbs$t.x,
  T.cal = aptcbs$T.cal)

# Draw posterior parameter distribution and posterior predictive distribution
pnbd.draws = pnbd.mcmc.DrawParameters(aptcbs) # gives an error
pnbd.xstar.draws = mcmc.DrawFutureTransactions(aptcbs, pnbd.draws)

# Collect the data
intervals_BA = data.table("Id" = unique(apparelTrans$Id),
                   "True" = aptcbs$x.star,
                   "Predicted" = aptcbs$xstar.pnbd,
                   "Lower" = 0,
                   "Upper" = 0
                   )

# Take the intervals of CET
for (i in 1:250){
  intervals_BA[i,4] = quantile(pnbd.xstar.draws[,i], probs = 0.05)
  intervals_BA[i,5] = quantile(pnbd.xstar.draws[,i], probs = 0.95)
  print(mean(pnbd.xstar.draws[,i]))
}

sum(intervals_BA$True > intervals_BA$Lower & intervals_BA$True < intervals_BA$Upper)/nrow(intervals_BA)


