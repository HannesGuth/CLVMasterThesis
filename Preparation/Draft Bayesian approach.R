print("1")
# data("apparelTrans")
# tryCatch(
#   {
    datecustomer = data2[,c(1,2)]
    colnames(datecustomer) = c("cust", "date")
    
    # Convert data into BTYD format
    data2cbs = elog2cbs(datecustomer, T.cal = min(datecustomer$date) + 7*splitweek2)
    params.pnbd = BTYD::pnbd.EstimateParameters(data2cbs[, c("x", "t.x", "T.cal")])
    BTYD::pnbd.cbs.LL(params.pnbd, data2cbs[, c("x", "t.x", "T.cal")])
    
    # Calculate CET
    if (whole_period2){
      data2cbs$xstar.pnbd = BTYD::pnbd.ConditionalExpectedTransactions(
        params = params.pnbd, T.star = as.numeric(difftime(max(data2$Date),min(datecustomer$date) + 7*splitweek2, units = "weeks")),
        x = data2cbs$x, t.x = data2cbs$t.x,
        T.cal = data2cbs$T.cal)
    }else{
      data2cbs$xstar.pnbd = BTYD::pnbd.ConditionalExpectedTransactions(
      params = params.pnbd, T.star = end2, # 39.85714 = as.numeric(difftime(as.Date("2005-01-03") + 40*7, max(apparelTrans$Date))) / 7
      x = data2cbs$x, t.x = data2cbs$t.x,
      T.cal = data2cbs$T.cal)
    }
    
    
    # Draw posterior parameter distribution and posterior predictive distribution
    pnbd.draws = pnbd.mcmc.DrawParameters(data2cbs) # gives an error
    pnbd.xstar.draws = mcmc.DrawFutureTransactions(data2cbs, pnbd.draws)
    
    # Collect the data
    intervals_BA = data.table("Id" = results_general$Id,
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
    for (i in 1:nrow(intervals_BA)){
      intervals_BA[i,2] = quantile(pnbd.xstar.draws[,i], probs = alpha/2)
      intervals_BA[i,3] = quantile(pnbd.xstar.draws[,i], probs = 1-(alpha/2))
      #intervals_BA[i,5] = quantile(pnbd.xstar.draws[,i], probs = 0.50) # assume the median as prediction
      #intervals_BA[i,5] = mean(pnbd.xstar.draws[,i])
      #print(mean(pnbd.xstar.draws[,i]))
    }
    intervals_BA$CET_true = results_general$actual.x
    intervals_BA$CET_prediction = results_general$CET
    intervals_BA$CET_covered = intervals_BA$CET_true >= intervals_BA$CET_lower & intervals_BA$CET_true <= intervals_BA$CET_upper
    
    # Performance measure
    mean(intervals_BA$CET_covered)
#   },
#   error = function(e){print(e)},#intervals_BA[,1:11] = NA
#   warning = function(w){print(w)}
# )



