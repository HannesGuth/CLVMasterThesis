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


#source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Data preparation.r")

rob_gift_list = list(data1 = gift1, data2 = gift2, s1 = c(20,60,160), s2 = c(40,60,120), pp = c(20,40,Inf), name = "gift")
rob_el_list = list(data1 = el1, data2 = el2, s1 = c(20,50,150), s2 = c(20,50,100), pp = c(20,40,Inf), name = "el")
robustness_list = list(el = rob_el_list)

for (rob_list in robustness_list){
  print(rob_list$name)
  data1 = rob_list$data1
  data2 = rob_list$data2

  big_grid = data.table(expand.grid(rob_list$s1, rob_list$s2, rob_list$pp, rob_list$pp))
  colnames(big_grid) = c("splitweek1", "splitweek2", "end1", "end2")
  big_grid$BS_CET = 0
  big_grid$EN_CET = 0
  big_grid$BA_CET = 0
  big_grid$QR_CET = 0
  big_grid$CP_CET = 0
  big_grid$CR_CET = 0
  big_grid$EN_PTS = 0
  big_grid$QR_PTS = 0
  big_grid$CP_PTS = 0
  big_grid$CR_PTS = 0
  big_grid$CET_CP_quantile = 0
  big_grid$PTS_CP_quantile = 0
  big_grid$CET_CR_quantile = 0
  big_grid$PTS_CR_quantile = 0
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
  big_grid = big_grid[order(big_grid$splitweek2, big_grid$end2),]
  
  for (run in 1:nrow(big_grid)){
    print(run)
   if ((big_grid[run,2] + big_grid[run,4]) != 160){
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
    
    if (run == 1 || ((run > 1) & (big_grid[run-1,2] != big_grid[run,2]) || (big_grid[run-1,4] != big_grid[run,4]))){
      # source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Bootstrapping periods.r")
      # print("Bootstrapping done")
      # print(big_grid)
      # source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Coefficients.r")
      # print("Ensemble done")
      # print(big_grid)
      source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Bayesian approach.r")
      print("Bayesian approach done")
      print(big_grid)
    }

    source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Quantile regression managerial.r")
    print("Quantile regression done")
    print(big_grid)
    source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Conformal prediction managerial one periods.r")
    print("Conformal prediction one done")
    print(big_grid)
    source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Conformal prediction managerial rep.r")
    print("Conformal prediction rep done")
    # big_grid[run, 5] = mean(intervals_BS$CET_covered, na.rm = TRUE)
    # big_grid[run, 6] = mean(intervals_EN$CET_covered, na.rm = TRUE)
    big_grid[run, 7] = mean(intervals_BA$CET_covered, na.rm = TRUE)
    big_grid[run, 8] = mean(intervals_QR_m$CET_covered, na.rm = TRUE)
    big_grid[run, 9] = mean(intervals_CP_m$CET_covered, na.rm = TRUE)
    big_grid[run, 10] = mean(intervals_CR_m$CET_covered, na.rm = TRUE)
    big_grid[run, 11] = mean(intervals_EN$PTS_covered, na.rm = TRUE)
    big_grid[run, 12] = mean(intervals_QR_m$PTS_covered, na.rm = TRUE)
    big_grid[run, 13] = mean(intervals_CP_m$PTS_covered, na.rm = TRUE)
    big_grid[run, 14] = mean(intervals_CR_m$PTS_covered, na.rm = TRUE)
    big_grid[run, 15] = quantile_CET_one
    big_grid[run, 16] = quantile_PTS_one
    big_grid[run, 17] = quantile_CET_rep
    big_grid[run, 18] = quantile_PTS_rep
    big_grid[run, 19] = as.numeric(grid[as.numeric(index_list[1]), 2])
    big_grid[run, 20] = as.numeric(grid[as.numeric(index_list[1]), 3])
    big_grid[run, 21] = as.numeric(grid[as.numeric(index_list[1]), 4])
    big_grid[run, 22] = as.numeric(grid[as.numeric(index_list[1]), 5])
    big_grid[run, 23] = as.numeric(grid[as.numeric(index_list[2]), 2])
    big_grid[run, 24] = as.numeric(grid[as.numeric(index_list[2]), 3])
    big_grid[run, 25] = as.numeric(grid[as.numeric(index_list[2]), 4])
    big_grid[run, 26] = as.numeric(grid[as.numeric(index_list[2]), 5])
    big_grid[run, 27] = as.numeric(grid[as.numeric(index_list[1]), 2])
    big_grid[run, 28] = as.numeric(grid[as.numeric(index_list[1]), 3])
    big_grid[run, 29] = as.numeric(grid[as.numeric(index_list[1]), 4])
    big_grid[run, 30] = as.numeric(grid[as.numeric(index_list[1]), 5])
    big_grid[run, 31] = as.numeric(grid[as.numeric(index_list[2]), 2])
    big_grid[run, 32] = as.numeric(grid[as.numeric(index_list[2]), 3])
    big_grid[run, 33] = as.numeric(grid[as.numeric(index_list[2]), 4])
    big_grid[run, 34] = as.numeric(grid[as.numeric(index_list[2]), 5])
    print(big_grid)
    path1 = paste0("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Backup/big_grid_", rob_list$name, ".RData")
    path2 = paste0("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Backup/big_grid_", rob_list$name, ".csv")
    saveRDS(big_grid , file = path1)
    write.csv(big_grid, file = path2)
    rob_list$results = big_grid
   }
  }
}

big_grid_gift = big_grid






write.csv(big_grid, "D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Miscellaneous/comparison_el.csv")

  
  
  
  
  
  
  






