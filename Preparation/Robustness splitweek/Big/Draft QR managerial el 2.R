
######### Managerial version
clv.el2 <- clvdata(el2,  
                     date.format="ymd", 
                     time.unit = "week",
                     estimation.split = splitweek2,
                     name.id = "Id",
                     name.date = "Date",
                     name.price = "Price")
est.el2 = pnbd(clv.data = clv.el2)

if (end2 > 0){
  results.el2 = predict(est.el2, predict.spending = TRUE, prediction.end = end2)
}else{
  results.el2 = predict(est.el2, predict.spending = TRUE)
}

# CET upper
est.el2@prediction.params.model[1] = as.numeric(grid[as.numeric(index_list[1]), 2])
est.el2@prediction.params.model[2] = as.numeric(grid[as.numeric(index_list[1]), 3])
est.el2@prediction.params.model[3] = as.numeric(grid[as.numeric(index_list[1]), 4])
est.el2@prediction.params.model[4] = as.numeric(grid[as.numeric(index_list[1]), 5])
pred = predict(est.el2, predict.spending = TRUE, prediction.end = end2)

if (end2 > 0){
  pred = predict(est.el2, predict.spending = TRUE, prediction.end = end2)
}else{
  pred = predict(est.el2, predict.spending = TRUE)
}

sum(pred$actual.x >= pred$CET)/nrow(pred)
CET_upper = pred$CET

# CET lower
est.el2@prediction.params.model[1] = as.numeric(grid[as.numeric(index_list[2]), 2])
est.el2@prediction.params.model[2] = as.numeric(grid[as.numeric(index_list[2]), 3])
est.el2@prediction.params.model[3] = as.numeric(grid[as.numeric(index_list[2]), 4])
est.el2@prediction.params.model[4] = as.numeric(grid[as.numeric(index_list[2]), 5])

if (end2 > 0){
  pred = predict(est.el2, predict.spending = TRUE, prediction.end = end2)
}else{
  pred = predict(est.el2, predict.spending = TRUE)
}

sum(pred$actual.x + CET_tolerance <= pred$CET)/nrow(pred)
CET_lower = pred$CET

# PTS upper
est.el2@prediction.params.model[1] = as.numeric(grid[as.numeric(index_list[3]), 2])
est.el2@prediction.params.model[2] = as.numeric(grid[as.numeric(index_list[3]), 3])
est.el2@prediction.params.model[3] = as.numeric(grid[as.numeric(index_list[3]), 4])
est.el2@prediction.params.model[4] = as.numeric(grid[as.numeric(index_list[3]), 5])

if (end2 > 0){
  pred = predict(est.el2, predict.spending = TRUE, prediction.end = end2)
}else{
  pred = predict(est.el2, predict.spending = TRUE)
}

sum(pred$actual.total.spending >= pred$predicted.total.spending)/nrow(pred)
PTS_upper = pred$predicted.total.spending

# PTS lower
est.el2@prediction.params.model[1] = as.numeric(grid[as.numeric(index_list[4]), 2])
est.el2@prediction.params.model[2] = as.numeric(grid[as.numeric(index_list[4]), 3])
est.el2@prediction.params.model[3] = as.numeric(grid[as.numeric(index_list[4]), 4])
est.el2@prediction.params.model[4] = as.numeric(grid[as.numeric(index_list[4]), 5])

if (end2 > 0){
  pred = predict(est.el2, predict.spending = TRUE, prediction.end = end2)
}else{
  pred = predict(est.el2, predict.spending = TRUE)
}

sum(pred$actual.total.spending + PTS_tolerance <= pred$predicted.total.spending)/nrow(pred)
PTS_lower = pred$predicted.total.spending

intervals_QR_m = data.table("Id" = results.el2$Id,
                          "CET_lower" = round(CET_lower, 4),
                          "CET_upper" = CET_upper,
                          "CET_true" = results.el2$actual.x,
                          "CET_prediction" = results.el2$CET,
                          "CET_covered" = results.el2$actual.x + CET_tolerance > CET_lower & results.el2$actual.x < CET_upper,
                          "PTS_lower" = round(PTS_lower, 4),
                          "PTS_upper" = PTS_upper,
                          "PTS_true" = results.el2$actual.total.spending,
                          "PTS_prediction" = results.el2$predicted.total.spending,
                          "PTS_covered" = results.el2$actual.total.spending + PTS_tolerance > PTS_lower & results.el2$actual.total.spending < PTS_upper
)


comparison[compcounter,9] = mean(intervals_QR_m$CET_covered)
comparison[compcounter,10] = mean(intervals_QR_m$PTS_covered)
comparison[compcounter,15] = as.numeric(grid[as.numeric(index_list[1]), 2])
comparison[compcounter,16] = as.numeric(grid[as.numeric(index_list[1]), 3])
comparison[compcounter,17] = as.numeric(grid[as.numeric(index_list[1]), 4])
comparison[compcounter,18] = as.numeric(grid[as.numeric(index_list[1]), 5])
comparison[compcounter,19] = as.numeric(grid[as.numeric(index_list[2]), 2])
comparison[compcounter,20] = as.numeric(grid[as.numeric(index_list[2]), 3])
comparison[compcounter,21] = as.numeric(grid[as.numeric(index_list[2]), 4])
comparison[compcounter,22] = as.numeric(grid[as.numeric(index_list[2]), 5])
comparison[compcounter,23] = as.numeric(grid[as.numeric(index_list[1]), 2])
comparison[compcounter,24] = as.numeric(grid[as.numeric(index_list[1]), 3])
comparison[compcounter,25] = as.numeric(grid[as.numeric(index_list[1]), 4])
comparison[compcounter,26] = as.numeric(grid[as.numeric(index_list[1]), 5])
comparison[compcounter,27] = as.numeric(grid[as.numeric(index_list[2]), 2])
comparison[compcounter,28] = as.numeric(grid[as.numeric(index_list[2]), 3])
comparison[compcounter,29] = as.numeric(grid[as.numeric(index_list[2]), 4])
comparison[compcounter,30] = as.numeric(grid[as.numeric(index_list[2]), 5])