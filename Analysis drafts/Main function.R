library(CLVTools)
library(data.table)
library(compiler)
library(ggplot2)
library(profvis)
library(rockchalk)
library(doParallel)
library(geomtextpath)
library(dbscan)

#splitweek = 130 # 130 for gift2
# Load data


source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Data preparation.r")

whole_period1 = FALSE
whole_period2 = FALSE

data1 = gift1
data2 = gift2
splitweek1 = 52
splitweek2 = 52
end1 = 52
end2 = 52

# data1 = el1
# data2 = el2
# splitweek1 = 52
# splitweek2 = 52
# end1 = 52
# end2 = 52

splitweek = c(26,52)
end = c(26,52,Inf)
big_grid = data.table(expand.grid(splitweek,splitweek,end,end))
colnames(big_grid) = c("splitweek1", "splitweek2", "end1", "end2")
big_grid$BS_CET = 0
big_grid$EN_CET = 0
big_grid$BA_CET = 0
big_grid$CP_CET = 0
big_grid$QR_CET = 0
big_grid$BS_PTS = 0
big_grid$EN_PTS = 0
big_grid$BA_PTS = 0
big_grid$CP_PTS = 0
big_grid$QR_PTS = 0
big_grid$CET_quantile = 0
big_grid$PTS_quantile = 0
big_grid$CET_Lr = 0
big_grid$CET_La = 0
big_grid$CET_Ls = 0
big_grid$CET_Lb = 0
big_grid$CET_Ur = 0
big_grid$CET_Ua = 0
big_grid$CET_Us = 0
big_grid$CET_Ub = 0
big_grid$PTS_Lr = 0
big_grid$PTS_La = 0
big_grid$PTS_Ls = 0
big_grid$PTS_Lb = 0
big_grid$PTS_Ur = 0
big_grid$PTS_Ua = 0
big_grid$PTS_Us = 0
big_grid$PTS_Ub = 0



for (run in 1:nrow(big_grid)){
  splitweek1 = as.numeric(big_grid[run,1])
  splitweek2 = as.numeric(big_grid[run,2])
  end1 = as.numeric(big_grid[run,3])
  end2 = as.numeric(big_grid[run,4])
  whole_period1 = ifelse(as.numeric(big_grid[run,3]) == Inf, TRUE, FALSE)
  whole_period2 = ifelse(as.numeric(big_grid[run,4]) == Inf, TRUE, FALSE)
  
  clv.data2 <- clvdata(data2,
                       date.format="ymd",
                       time.unit = "week",
                       estimation.split = splitweek2,
                       name.id = "Id",
                       name.date = "Date",
                       name.price = "Price")
  
  # Estimate standard Pareto/NBD Model
  est.data2 <- pnbd(clv.data = clv.data2, verbose = TRUE)
  if (whole_period2){
    results_general <- predict(est.data2, predict.spending = TRUE)
  }else{
    results_general <- predict(est.data2, predict.spending = TRUE, prediction.end = end2)
  }
  
  print(results_general)
  
  # source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Bootstrapping.r")
  # print("Bootstrapping done")
  source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Coefficients.r")
  print("Ensemble done")
  source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Bayesian approach.r")
  print("Bayesian approach done")
  source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Quantile regression managerial.r")
  print("Quantile regression done")
  source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Conformal prediction managerial Bias 2.r")
  print("Conformal prediction done")
  #source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Benchmarking.r")
  #source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Plotting.r")
  #big_grid[run, 5] = mean(intervals_BS$CET_covered)
  big_grid[run, 6] = mean(intervals_EN$CET_covered)
  big_grid[run, 7] = mean(intervals_BA$CET_covered)
  big_grid[run, 8] = mean(intervals_CP_m$CET_covered)
  big_grid[run, 9] = mean(intervals_QR_m$CET_covered)
  #big_grid[run, 10] = mean(intervals_BS$PTS_covered)
  big_grid[run, 11] = mean(intervals_EN$PTS_covered)
  #big_grid[run, 12] = mean(intervals_BA$PTS_covered)
  big_grid[run, 13] = mean(intervals_CP_m$PTS_covered)
  big_grid[run, 14] = mean(intervals_QR_m$PTS_covered)
  big_grid[run, 15] = quantile_CET
  big_grid[run, 16] = quantile_PTS
  big_grid[run, 17] = as.numeric(grid[as.numeric(index_list[1]), 2])
  big_grid[run, 18] = as.numeric(grid[as.numeric(index_list[1]), 3])
  big_grid[run, 19] = as.numeric(grid[as.numeric(index_list[1]), 4])
  big_grid[run, 20] = as.numeric(grid[as.numeric(index_list[1]), 5])
  big_grid[run, 21] = as.numeric(grid[as.numeric(index_list[2]), 2])
  big_grid[run, 22] = as.numeric(grid[as.numeric(index_list[2]), 3])
  big_grid[run, 23] = as.numeric(grid[as.numeric(index_list[2]), 4])
  big_grid[run, 24] = as.numeric(grid[as.numeric(index_list[2]), 5])
  big_grid[run, 25] = as.numeric(grid[as.numeric(index_list[1]), 2])
  big_grid[run, 26] = as.numeric(grid[as.numeric(index_list[1]), 3])
  big_grid[run, 27] = as.numeric(grid[as.numeric(index_list[1]), 4])
  big_grid[run, 28] = as.numeric(grid[as.numeric(index_list[1]), 5])
  big_grid[run, 29] = as.numeric(grid[as.numeric(index_list[2]), 2])
  big_grid[run, 30] = as.numeric(grid[as.numeric(index_list[2]), 3])
  big_grid[run, 31] = as.numeric(grid[as.numeric(index_list[2]), 4])
  big_grid[run, 32] = as.numeric(grid[as.numeric(index_list[2]), 5])
  print(big_grid)
}

write.csv(big_grid, "D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Miscellaneous/comparison_el.csv")

  
  
  
  
  
  
  






