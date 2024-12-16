write("INTERVAL GENERATION", file = paste0(getwd(), "/Status report.txt"), append = TRUE)
packages = c(
  "CLVTools", "data.table", "compiler", "ggplot2", "profvis", 
  "rockchalk", "doParallel", "geomtextpath", "dbscan", 
  "tidyr", "gghalves", "BTYD", "BTYDplus", "fmsb", "bit64", "scales"
)

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

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
library(bit64)
library(scales)

# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load data
source(paste0(getwd(), "/Data preparation.r"))
write(paste("Data preparation done"), file = paste0(getwd(), "/Status report.txt"), append = TRUE)


# Set defaults
alpha = 0.1
methodlist = c("BS", "EN", "BA", "QR", "CP", "CR")
time_table = data.table("Method" = methodlist,
                        "gift" = 0,
                        "el" = 0,
                        "multi" = 0,
                        "apparel" = 0)
times = c(0,0,0,0,0,0)
all_res = list()

data_lists2 = list(gift_list = gift_list, el_list = el_list) #, multi_list = multi_list, apparel_list = apparel_list)
for (data_list in data_lists2){
  set.seed(1)
  print(paste(data_list$name, "(Main function one)"))
  data1 = data_list$data1
  data2 = data_list$data2
  splitweek1 = data_list$l1
  splitweek2 = data_list$l2
  end1 = data_list$p1
  end2 = data_list$p2
  whole_period1 = data_list$wp1
  whole_period2 = data_list$wp2
  
  clv.data2 = clvdata(data2,
                       date.format="ymd",
                       time.unit = "week",
                       estimation.split = splitweek2,
                       name.id = "Id",
                       name.date = "Date",
                       name.price = "Price")
  
  # Estimate standard Pareto/NBD Model
  est.data2 = pnbd(clv.data = clv.data2, verbose = TRUE, optimx.args = list(method = "Nelder-Mead"))
  if (whole_period2){
    results_general = predict(est.data2, predict.spending = TRUE)
  }else{
    results_general = predict(est.data2, predict.spending = TRUE, prediction.end = end2)
  }
  print(results_general)
  
  
  if (data_list$BS){
    time1 = Sys.time()
    set.seed(1)
    source(paste0(getwd(), "/BS.r"))
    time2 = Sys.time()
    times[1] = as.numeric(difftime(time2, time1, units = "secs"))
  }else{
    #intervals_BS[,2:11]
    times[1] = NA #as.numeric(difftime(time2, time1, units = "secs"))
  }
  print(paste("Bootstrapping done for", data_list$name))
  write(paste("   Bootstrapping done for", data_list$name), file = paste0(getwd(), "/Status report.txt"), append = TRUE)
  time1 = Sys.time()
  set.seed(1)
  source(paste0(getwd(), "/EN.r"))
  time2 = Sys.time()
  times[2] = as.numeric(difftime(time2, time1, units = "secs"))
  print(paste("Ensemble done for", data_list$name))
  write(paste("   Ensemble done for", data_list$name), file = paste0(getwd(), "/Status report.txt"), append = TRUE)
  time1 = Sys.time()
  set.seed(1)
  source(paste0(getwd(), "/BA.r"))
  time2 = Sys.time()
  times[3] = as.numeric(difftime(time2, time1, units = "secs"))
  print(paste("Bayesian approach done for", data_list$name))
  write(paste("   Bayesian approach done for", data_list$name), file = paste0(getwd(), "/Status report.txt"), append = TRUE)
  time1 = Sys.time()
  set.seed(1)
  source(paste0(getwd(), "/QR.r"))
  time2 = Sys.time()
  times[4] = as.numeric(difftime(time2, time1, units = "secs"))
  print(paste("Quantile regression done for", data_list$name))
  write(paste("   Quantile regression done for", data_list$name), file = paste0(getwd(), "/Status report.txt"), append = TRUE)
  time1 = Sys.time()
  set.seed(1)
  source(paste0(getwd(), "/CP one.r"))
  time2 = Sys.time()
  times[5] = as.numeric(difftime(time2, time1, units = "secs"))
  print(paste("Conformal prediction one done for", data_list$name))
  write(paste("   Conformal prediction done for", data_list$name), file = paste0(getwd(), "/Status report.txt"), append = TRUE)
  time1 = Sys.time()
  set.seed(1)
  source(paste0(getwd(), "/CR.r"))
  time2 = Sys.time()
  times[6] = as.numeric(difftime(time2, time1, units = "secs"))
  print(paste("Conformal prediction rep done for", data_list$name))
  write(paste("   Conformal prediction rep done for", data_list$name), file = paste0(getwd(), "/Status report.txt"), append = TRUE)
  set.seed(1)
  source(paste0(getwd(), "/Benchmarking.r"))
  write(paste("   Benchmarking done for", data_list$name), file = paste0(getwd(), "/Status report.txt"), append = TRUE)
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
  saveRDS(all_res, file = paste0(getwd(), "/Results/all_res.RData"))
  time_table[[data_list$name]] = times
  
  write(paste("All methods done for", data_list$name), file = paste0(getwd(), "/Status report.txt"), append = TRUE)
}

source(paste0(getwd(), "/Data reorganization.r"))
print("Data reorganization done")
write("Data reorganization done", file = paste0(getwd(), "/Status report.txt"), append = TRUE)
source(paste0(getwd(), "/Results analysis.r"))
print("Results analysis done")
write("Results analysis done", file = paste0(getwd(), "/Status report.txt"), append = TRUE)
source(paste0(getwd(), "/Application in marketing.r"))
print("Application in marketing")
write("Application in marketing", file = paste0(getwd(), "/Status report.txt"), append = TRUE)
source(paste0(getwd(), "/Plotting.r"))
print("Plotting done")
write("Plotting done", file = paste0(getwd(), "/Status report.txt"), append = TRUE)
source(paste0(getwd(), "/Dataset analysis.r"))
print("Dataset analysis done")
write("Dataset analysis done", file = paste0(getwd(), "/Status report.txt"), append = TRUE)
source(paste0(getwd(), "/Covariates simulated.r"))
print("Covariates simulated done")
write("Covariates simulated done", file = paste0(getwd(), "/Status report.txt"), append = TRUE)
source(paste0(getwd(), "/Comparison.r"))
print("Comparison done")
write("Comparison done", file = paste0(getwd(), "/Status report.txt"), append = TRUE)
