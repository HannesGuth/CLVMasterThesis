

clv.gift2 <- clvdata(gift2,
                     date.format="ymd", 
                     time.unit = "week",
                     estimation.split = splitweek2, # 15, 50, 80, 100
                     name.id = "Id",
                     name.date = "Date",
                     name.price = "Price")
est.gift2 = pnbd(clv.data = clv.gift2)

if (end2 > 0){
  results.gift2 = predict(est.gift2, predict.spending = TRUE, prediction.end = end2)
}else{
  results.gift2 = predict(est.gift2, predict.spending = TRUE)
}


CET_quantile = mean(as.vector(unlist(quantiles_CET)))
PTS_quantile = mean(as.vector(unlist(quantiles_PTS)))

CET_std = predict(CET_mod, data.frame(CET_pred = results.gift2$CET))
PTS_std = predict(PTS_mod, data.frame(PTS_pred = results.gift2$predicted.total.spending))

intervals_CP_m = data.table("Id" = results.gift2$Id,
                          "CET_lower" = results.gift2$CET - (CET_std * CET_quantile),
                          "CET_upper" = results.gift2$CET + (CET_std * CET_quantile),
                          "CET_true" = results.gift2$actual.x,
                          "CET_prediction" = results.gift2$CET,
                          "CET_covered" = 0,
                          "PTS_lower" = results.gift2$predicted.total.spending - (PTS_std * CET_quantile),
                          "PTS_upper" = results.gift2$predicted.total.spending + (PTS_std * CET_quantile),
                          "PTS_true" = results.gift2$actual.total.spending,
                          "PTS_prediction" = results.gift2$predicted.total.spending,
                          "PTS_covered" = 0
)

intervals_CP_m$CET_covered = intervals_CP_m$CET_lower <= intervals_CP_m$CET_true & intervals_CP_m$CET_upper >= intervals_CP_m$CET_true
intervals_CP_m$PTS_covered = intervals_CP_m$PTS_lower <= intervals_CP_m$PTS_true & intervals_CP_m$PTS_upper >= intervals_CP_m$PTS_true

comparison[compcounter,5] = mean(intervals_CP_m$CET_covered)
comparison[compcounter,6] = mean(intervals_CP_m$PTS_covered)
comparison[compcounter,9] = CET_quantile
comparison[compcounter,10] = PTS_quantile