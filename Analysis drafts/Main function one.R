library(CLVTools)
library(data.table)
library(compiler)
library(ggplot2)
library(profvis)
library(rockchalk)
library(doParallel)
library(geomtextpath)
library(dbscan)
library(tidyr)
library(gghalves)
library(BTYD)
library(BTYDplus)
library(fmsb)

#splitweek = 130 # 130 for gift2
# Load data


source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Data preparation.r")
# 
# whole_period1 = FALSE
# whole_period2 = FALSE
# 
# data1 = el1
# data2 = el2
# splitweek1 = 52
# splitweek2 = 52
# end1 = 52
# end2 = 52
# 
# clv.data2 <- clvdata(data2,
#                      date.format="ymd",
#                      time.unit = "week",
#                      estimation.split = splitweek2,
#                      name.id = "Id",
#                      name.date = "Date",
#                      name.price = "Price")
# 
# # Estimate standard Pareto/NBD Model
# est.data2 <- pnbd(clv.data = clv.data2, verbose = TRUE)
# if (whole_period2){
#   results_general <- predict(est.data2, predict.spending = TRUE)
# }else{
#   results_general <- predict(est.data2, predict.spending = TRUE, prediction.end = end2)
# }
# 
# print(results_general)
# 
# intervals_methods = c("BS", "EN", "BA", "QR", "CP", "CR")
# time_table = data.table("Method" = intervals_methods,
#                         "gift" = 0,
#                         "el" = 0,
#                         "multi" = 0,
#                         "apparel" = 0)
# times = c(0,0,0,0,0,0)
# 
# time1 = Sys.time()
# source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Bootstrapping.r")
# time2 = Sys.time()
# times[1] = difftime(time2, time1)
# print("Bootstrapping done")
# time1 = Sys.time()
# source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Coefficients.r")
# time2 = Sys.time()
# times[2] = difftime(time2, time1)
# print("Ensemble done")
# time1 = Sys.time()
# source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Bayesian approach.r")
# time2 = Sys.time()
# times[3] = difftime(time2, time1)
# print("Bayesian approach done")
# time1 = Sys.time()
# source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Quantile regression managerial.r")
# time2 = Sys.time()
# times[4] = difftime(time2, time1)
# print("Quantile regression done")
# time1 = Sys.time()
# source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Conformal prediction managerial one.r")
# time2 = Sys.time()
# times[5] = difftime(time2, time1)
# print("Conformal prediction done")
# time1 = Sys.time()
# source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Conformal prediction managerial rep.r")
# time2 = Sys.time()
# times[6] = difftime(time2, time1)
# print("Conformal prediction rep done")
# source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Benchmarking.r")
# #source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Plotting.r")
# 
# time_table$apparel = times
# gift_results = list(intervals_BS = intervals_BS,
#                     intervals_EN = intervals_EN,
#                     intervals_BA = intervals_BA,
#                     intervals_QR_m = intervals_QR_m,
#                     intervals_CP_m = intervals_CP_m,
#                     intervals_CR_m = intervals_CR_m,
#                     CET_measures = CET_measures,
#                     PTS_measures = PTS_measures)
# 
# el_results = list(intervals_BS = intervals_BS,
#                   intervals_EN = intervals_EN,
#                   intervals_BA = intervals_BA,
#                   intervals_QR_m = intervals_QR_m,
#                   intervals_CP_m = intervals_CP_m,
#                   intervals_CR_m = intervals_CR_m,
#                   CET_measures = CET_measures,
#                   PTS_measures = PTS_measures)
# 
# multi_results = list(intervals_BS = intervals_BS,
#                      intervals_EN = intervals_EN,
#                      intervals_BA = intervals_BA,
#                      intervals_QR_m = intervals_QR_m,
#                      intervals_CP_m = intervals_CP_m,
#                      intervals_CR_m = intervals_CR_m,
#                      CET_measures = CET_measures,
#                      PTS_measures = PTS_measures)
# 
# apparel_results = list(intervals_BS = intervals_BS,
#                   intervals_EN = intervals_EN,
#                   intervals_BA = intervals_BA,
#                   intervals_QR_m = intervals_QR_m,
#                   intervals_CP_m = intervals_CP_m,
#                   intervals_CR_m = intervals_CR_m,
#                   CET_measures = CET_measures,
#                   PTS_measures = PTS_measures)
# 
# all_results = list(gift_results = gift_results,
#                    el_results = el_results,
#                    multi_results = multi_results,
#                    apparel_results = apparel_results)
# 
# save.image()
# 
# write.csv(big_grid, "D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Miscellaneous/comparison_el.csv")
# 
# 
# rename_in_list <- function(lst, old_name, new_name) {
#   if (old_name %in% names(lst)) {
#     lst[[new_name]] <- lst[[old_name]]
#     lst[[old_name]] <- NULL
#   }
#   return(lst)
# }
# 
# # Apply renaming function to each nested list within all_results
# all_results <- lapply(all_results, rename_in_list, old_name = "intervals_CP_m_rep", new_name = "intervals_CP_m")
# 
# # Check the structure to verify renaming
# str(all_results)




#####
alpha = 0.1
intervals_methods = c("BS", "EN", "BA", "QR", "CP", "CR")
time_table = data.table("Method" = intervals_methods,
                        "gift" = 0,
                        "el" = 0,
                        "multi" = 0,
                        "apparel" = 0)
times = c(0,0,0,0,0,0)
all_res = list()

data_lists2 = list(gift_list = gift_list, el_list = el_list, multi_list = multi_list, apparel_list = apparel_list)
for (data_list in data_lists2){
  print(data_list$name)
  data1 = data_list$data1
  data2 = data_list$data2
  splitweek1 = data_list$l1
  splitweek2 = data_list$l2
  end1 = data_list$p1
  end2 = data_list$p2
  whole_period1 = data_list$wp1
  whole_period2 = data_list$wp2
  
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
  
  
  if (data_list$BS){
    time1 = Sys.time()
    source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Bootstrapping.r")
    time2 = Sys.time()
    times[1] = as.numeric(difftime(time2, time1, units = "secs"))
  }else{
    intervals_BS[,2:11]
    times[1] = NA #as.numeric(difftime(time2, time1, units = "secs"))
  }
  print(paste("Bootstrapping done for", data_list$name))
  time1 = Sys.time()
  source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Coefficients.r")
  time2 = Sys.time()
  times[2] = as.numeric(difftime(time2, time1, units = "secs"))
  print(paste("Ensemble done for", data_list$name))
  time1 = Sys.time()
  source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Bayesian approach.r")
  time2 = Sys.time()
  times[3] = as.numeric(difftime(time2, time1, units = "secs"))
  print(paste("Bayesian appraoch done for", data_list$name))
  time1 = Sys.time()
  source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Quantile regression managerial.r")
  time2 = Sys.time()
  times[4] = as.numeric(difftime(time2, time1, units = "secs"))
  print(paste("Quantile regression done for", data_list$name))
  time1 = Sys.time()
  source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Conformal prediction managerial one.r")
  time2 = Sys.time()
  times[5] = as.numeric(difftime(time2, time1, units = "secs"))
  print(paste("Conformal prediction one done for", data_list$name))
  time1 = Sys.time()
  source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Conformal prediction managerial rep.r")
  time2 = Sys.time()
  times[6] = as.numeric(difftime(time2, time1, units = "secs"))
  print(paste("Conformal prediction rep done for", data_list$name))
  source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Benchmarking.r")
  name = as.character(data_list$name)
  time_table[[name]] = times
  
  int_res = list(intervals_BS = intervals_BS,
                 intervals_EN = intervals_EN,
                 intervals_BA = intervals_BA,
                 intervals_QR_m = intervals_QR_m,
                 intervals_CP_m = intervals_CP_m,
                 intervals_CR_m = intervals_CR_m,
                 CET_measures = CET_measures,
                 PTS_measures = PTS_measures,
                 comp_time = times)
  
  all_res[[data_list$name]] = int_res
  saveRDS(all_res, file = "D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Backup/all_res_times.RData")
  time_table[[data_list$name]] = times
}

saveRDS(all_res_backup , file="D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Backup/all_res.RData")
