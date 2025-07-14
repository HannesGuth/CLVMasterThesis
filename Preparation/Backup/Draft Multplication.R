# install.packages("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/CLVTools", repos = NULL, type="source")
# 
# # Load package
# library(CLVTools)
# library(data.table)
# library(compiler)
# library(ggplot2)
# library(profvis)
# library(rockchalk)
# 
# # Load data
# data("apparelTrans")
# clv.apparel <- clvdata(apparelTrans,  
#                        date.format="ymd", 
#                        time.unit = "week",
#                        estimation.split = 40,
#                        name.id = "Id",
#                        name.date = "Date",
#                        name.price = "Price")
# 
# # Estimate standard Pareto/NBD Model
# est.pnbd <- pnbd(clv.data = clv.apparel, verbose = TRUE)
# summary(est.pnbd)
# results <- predict(est.pnbd, predict.spending = TRUE)
# print(results)
# est.gg <- gg(clv.data = clv.apparel)
# predict(est.gg)
# 
# # Boostrapping to get prediction intervalls
# set.seed(1)
# results_boots <- predict(est.pnbd, uncertainty="boots")
# 
# # A more detailed look at the results
# results_boots$predicted.CLV
# results_boots$predicted.CLV.CI.5
# results_boots$predicted.CLV.CI.95
# results_boots$actual.total.spending
# hist((results_boots$predicted.CLV-results_boots$predicted.CLV.CI.5)/results_boots$predicted.CLV*100)
# hist((results_boots$predicted.CLV.CI.95-results_boots$predicted.CLV)/results_boots$predicted.CLV*100)

# Multiplication approach
options(scipen=999)
mean(((results_boots$DERT.CI.95 - results_boots$DERT) - (results_boots$DERT - results_boots$DERT.CI.5))/results_boots$DERT)

x = rnorm(100000, mean = (results_boots$DERT.CI.5[1] + results_boots$DERT.CI.95[1]) / 2, sd = (results_boots$DERT.CI.95[1] - results_boots$DERT.CI.5[1]) / (2 * qnorm(0.975)))
hist(x, breaks = 200)

MB_DERT = data.table("Run" = 1:1000)
MB_PMS = data.table("Run" = 1:1000)
MB_CLV = data.table("Run" = 1:1000000)

# Sample for each customer 1000 DERTs and predicted mean spendings, assuming they were normally distributed
for (i in 1:250){
  column = toString(results_boots$Id[i])
  MB_DERT[, (column) := rnorm(1000, mean = (results_boots$DERT.CI.5[i] + results_boots$DERT.CI.95[i]) / 2, sd = (results_boots$DERT.CI.95[i] - results_boots$DERT.CI.5[i]) / (2 * qnorm(0.975)))]
  MB_PMS[, (column) := rnorm(1000, mean = (results_boots$predicted.mean.spending.CI.5[i] + results_boots$predicted.mean.spending.CI.95[i]) / 2, sd = (results_boots$predicted.mean.spending.CI.95[i] - results_boots$predicted.mean.spending.CI.5[i]) / (2 * qnorm(0.975)))]
}

# Calculate 1000 CLVs for every customers
#MB_CLV = MB_DERT * MB_PMS
MB_DERT = data.frame(MB_DERT)
MB_PMS = data.frame(MB_PMS)
MB_CLV = data.frame(matrix(NA, nrow = 1000000, ncol = 251))
for (i in 2:251){
  MB_CLV[,i] = as.vector(outer(as.vector(as.numeric(unlist(MB_PMS[,i]))), as.vector(as.numeric(unlist(MB_DERT[,i]))), "*"))
}

# Summarize the data in a table
intervals_MB = data.table("Id" = results_boots$Id,
                          "Mod_DERT" = results_boots$DERT,
                          "Mod_DERT05" = results_boots$DERT.CI.5,
                          "Mod_DERT95" = results_boots$DERT.CI.95,
                          "Mod_PMS" = results_boots$predicted.mean.spending,
                          "Mod_PMS05" = results_boots$predicted.mean.spending.CI.5,
                          "Mod_PMS95" = results_boots$predicted.mean.spending.CI.95,
                          "MB_CLV_05%" = 0,
                          "MB_CLV_95%" = 0,
                          "CLV_05" = results_boots$predicted.CLV.CI.5,
                          "CLV_95" = results_boots$predicted.CLV.CI.95,
                          "CLV" = results_boots$predicted.CLV
)

MB_CLV = data.table(MB_CLV)
# Take the quantiles
for (i in 2:251){
  intervals_MB[i-1,8] = quantile(MB_CLV[,..i], probs = c(0.05,0.95), na.rm = TRUE)[1]
  intervals_MB[i-1,9] = quantile(MB_CLV[,..i], probs = c(0.05,0.95), na.rm = TRUE)[2]
}
intervals_MB
rm(MB_CLV)

#########
# 
# # For check only
# plot_data_old = data.table("Id" = results_boots$Id,
#                        "mod_PI_05" = results_boots$predicted.CLV.CI.5,
#                        "mod_PI_95" = results_boots$predicted.CLV.CI.95,
#                        "PB_PI_05" = intervals_PB$`PB_CLV_05%`,
#                        "PB_PI_95" = intervals_PB$`PB_CLV_95%`,
#                        "MB_PI_05" = intervals_MB$`MB_CLV_05%`,
#                        "MB_PI_95" = intervals_MB$`MB_CLV_95%`,
#                        "CP_PI_05" = intervals_CP$Lower,
#                        "CP_PI_95" = intervals_CP$Upper,
#                        "mod_CLV" = results_boots$predicted.CLV,
#                        "true" = new3$CLV
#                        )
# 
# covered_mod = sum(plot_data_old$true < plot_data_old$mod_PI_95 & plot_data_old$true > plot_data_old$mod_PI_05)
# covered_PB = sum(plot_data_old$true < plot_data_old$PB_PI_95 & plot_data_old$true > plot_data_old$PB_PI_05)
# covered_MB = sum(plot_data_old$true < plot_data_old$MB_PI_95 & plot_data_old$true > plot_data_old$MB_PI_05)
# covered_CP = sum(plot_data_old$true < plot_data_old$CP_PI_95 & plot_data_old$true > plot_data_old$CP_PI_05)
# 
# # Is any point prediction outside of any interval?
# plot_data_old$YN = (plot_data_old$mod_CLV > plot_data_old$mod_PI_95) + (plot_data_old$mod_CLV > plot_data_old$PB_PI_95) + (plot_data_old$mod_CLV > plot_data_old$MB_PI_95)
# plot_data_old$NY = (plot_data_old$mod_CLV < plot_data_old$mod_PI_05) + (plot_data_old$mod_CLV < plot_data_old$PB_PI_05) + (plot_data_old$mod_CLV < plot_data_old$MB_PI_05)
# sum(plot_data_old$YN)
# sum(plot_data_old$NY)
# 
# # Prepare data to plot
# plot_data = data.table("Id" = rep(results_boots$Id, each = 4),
#                          "Method" = rep(c("PI from CLVTools","Method 1","Method 2","Conformal prediction"), n = 250),
#                          "Low" = c(rbind(results_boots$predicted.CLV.CI.5, intervals_PB$`PB_CLV_05%`, intervals_MB$`MB_CLV_05%`, intervals_CP$Lower)),
#                          "High" = c(rbind(results_boots$predicted.CLV.CI.95, intervals_PB$`PB_CLV_95%`, intervals_MB$`MB_CLV_95%`, intervals_CP$Upper)),
#                          "CLV" = rep(results_boots$predicted.CLV, each = 4)
#                          )
# 
# ggplot(plot_data[plot_data$CLV >= 50 & plot_data$CLV <= 500,], aes(as.factor(Id), CLV)) +
#   geom_pointrange(
#     aes(ymin = Low, ymax = High, color = Method),
#     position = position_dodge(0.3)
#   ) +
#   labs(title = "90% PIs for customers with predicted CLV in [29,32]", x = "Customer", y = "CLV")# +
#   ylim(0,60)
# 
# # Actually not needed anymore
# plot_data_new = data.table("Id" = c(1,1,1,10,10,10),
#                            "Method" = c("a","b","c","a","b","c"),
#                            "Low" = c(2.027,1.158,2.054,10.334,9.916,9.853),
#                            "High" = c(3.049,2.871,3.042,17.727,18.498,17.169),
#                            "CLV" = c(2.445, 2.445, 2.445, 14.133, 14.133, 14.133)
#                            )
# 
# c(rbind(results_boots$predicted.CLV.CI.5, intervals_PB$`PB_CLV_05%`, intervals_MB$`MB_CLV_05%`))
# 
# #####
# est.pnbd <- pnbd(clv.data = clv.apparel, verbose = TRUE)
# results_pnbd <- predict(est.pnbd, predict.spending = TRUE)
# est.ggomnbd <- ggomnbd(clv.data = clv.apparel, verbose = TRUE)
# results_ggomnbd <- predict(est.ggomnbd, predict.spending = TRUE)
# est.bgnbd <- bgnbd(clv.data = clv.apparel, verbose = TRUE)
# results_bgnbd <- predict(est.bgnbd, predict.spending = TRUE)