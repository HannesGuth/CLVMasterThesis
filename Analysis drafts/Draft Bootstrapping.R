# Bootstrapping to get prediction intervals
results_boots = predict(est.gift2, uncertainty="boots")

intervals_BS = data.table("Id" = results_general$Id,
                          "CET_lower" = results_boots$CET.CI.5,
                          "CET_upper" = results_boots$CET.CI.95,
                          "CET_true" = results_general$actual.x,
                          "CET_prediction" = results_general$CET,
                          "CET_covered" = results_boots$actual.x > results_boots$CET.CI.5 & results_boots$actual.x < results_boots$CET.CI.95,
                          "PTS_lower" = NA,
                          "PTS_upper" = NA,
                          "PTS_true" = results_general$actual.total.spending,
                          "PTS_prediction" = results_general$predicted.total.spending,
                          "PTS_covered" = NA
)
