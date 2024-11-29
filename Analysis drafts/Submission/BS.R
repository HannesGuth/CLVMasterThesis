clv.data2 <- clvdata(data2,
                     date.format="ymd",
                     time.unit = "week",
                     estimation.split = splitweek2,
                     name.id = "Id",
                     name.date = "Date",
                     name.price = "Price")

# Estimate standard Pareto/NBD Model
est.data2 <- pnbd(clv.data = clv.data2, verbose = TRUE)

# Bootstrapping to get prediction intervals
if (whole_period2){
  results_boots = predict(est.data2, uncertainty="boots", num.boots = 100)
}else{
  results_boots = predict(est.data2, uncertainty="boots", prediction.end = end2, num.boots = 100)
}

intervals_BS = data.table("Id" = results_boots$Id,
                          "CET_lower" = results_boots$CET.CI.5,
                          "CET_upper" = results_boots$CET.CI.95,
                          "CET_true" = results_boots$actual.x,
                          "CET_prediction" = results_boots$CET,
                          "CET_covered" = results_boots$actual.x > results_boots$CET.CI.5 & results_boots$actual.x < results_boots$CET.CI.95,
                          "PTS_lower" = NA,
                          "PTS_upper" = NA,
                          "PTS_true" = results_boots$actual.total.spending,
                          "PTS_prediction" = results_boots$predicted.total.spending,
                          "PTS_covered" = NA
)
