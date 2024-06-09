# install.packages("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/CLVTools", repos = NULL, type="source")
# 
# # Load package
# library(CLVTools)
# library(data.table)
# library(compiler)
# library(ggplot2)
# library(profvis)
# library(rockchalk)
# library(TAF)
# 
# splitWeek = 40
# 
# # Load data
# data("apparelTrans")
# clv.apparel <- clvdata(apparelTrans,  
#                        date.format="ymd", 
#                        time.unit = "week",
#                        estimation.split = splitWeek,
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

# Benchmarking

benchmark_CET = data.table("Id" = results_boots$Id,
                           "mod_CET_PI_05" = results_boots$CET.CI.5,
                           "mod_CET_PI_95" = results_boots$CET.CI.95,
                           "PB_CET_PI_05" = intervals_PB$PB_CET_05,
                           "PB_CET_PI_95" = intervals_PB$PB_CET_95,
                           "CP_CET_PI_05" = intervals_CP$CET_Lower,
                           "CP_CET_PI_95" = intervals_CP$CET_Upper,
                           "BA_CET_PI_05" = intervals_BA$Lower,
                           "BA_CET_PI_95" = intervals_BA$Upper,
                           "QR_CET_PI_05" = intervals_QR$CET_lower,
                           "QR_CET_PI_95" = intervals_QR$CET_upper,
                           "pred" = results$CET,
                           "true" = results$actual.x,
                           "mod_CLV" = results_boots$predicted.CLV
)

benchmark_PTS = data.table("Id" = results_boots$Id,
                           "mod_PTS_PI_05" = 0,
                           "mod_PTS_PI_95" = 0,
                           "PB_PTS_PI_05" = intervals_PB$PB_PTS_05,
                           "PB_PTS_PI_95" = intervals_PB$PB_PTS_95,
                           "CP_PTS_PI_05" = intervals_CP$PTS_Lower,
                           "CP_PTS_PI_95" = intervals_CP$PTS_Upper,
                           "QR_PTS_PI_05" = intervals_QR$CET_lower,
                           "QR_PTS_PI_95" = intervals_QR$CET_upper,
                           "pred" = results$predicted.total.spending,
                           "true" = results$actual.total.spending,
                           "mod_CLV" = results_boots$predicted.CLV
)

#benchmark_data$true = benchmark_data$mod_CLV + (benchmark_data$mod_CLV * runif(nrow(benchmark_data), -0.2, 0.2))
#benchmark_data$true = new3$CLV

measures_CET = data.table("Measure" = c("PICP", "ACE", "Upper coverage", "Lower coverage", "MIS", "Bias", "MSPIW", "MSPIWW", "SWR"))
measures_PTS = data.table("Measure" = c("PICP", "ACE", "Upper coverage", "Lower coverage", "MIS", "Bias", "MSPIW", "MSPIWW", "SWR"))
quantiles = quantile(benchmark_CET$mod_CLV, c(1/3,2/3))
s1 = benchmark_CET[mod_CLV < quantiles[1]]
s2 = benchmark_CET[mod_CLV >= quantiles[1] & mod_CLV < quantiles[2]]
s3 = benchmark_CET[mod_CLV >= quantiles[2]]
items = list(s1 = s1, s2 = s2, s3 = s3)
counter = 0
methods = list("BS", "M1", "CP", "BA", "QR")
rownames_CET = list()
alpha = 0.05
x = 0

# lower = list(item$mod_CET_PI_05, item$PB_CET_PI_05, item$CP_CET_PI_05)
# upper = list(item$mod_CET_PI_95, item$PB_CET_PI_95, item$CP_CET_PI_95)

for (item in items){
  counter = counter + 1
  lower = list(item$mod_CET_PI_05, item$PB_CET_PI_05, item$CP_CET_PI_05, item$BA_CET_PI_05, item$QR_CET_PI_05)
  upper = list(item$mod_CET_PI_95, item$PB_CET_PI_95, item$CP_CET_PI_95, item$BA_CET_PI_95, item$QR_CET_PI_95)
  
  for (j in 1:5){
    x = x + 1
    values = list()
    #print(methods[j])
    # Coverage/PICP (16PI, 20PI)
    PICP = sum(item$true < unlist(upper[j]) & item$true > unlist(lower[j])) / nrow(item)
    values = c(values, PICP)
    
    # Average coverage error (16PI, 20PI)
    values = c(values, 0.90 - PICP)

    # Upper coverage (2L)
    values = c(values, sum(item$true < unlist(unlist(upper[j]))) / nrow(item))
    
    # Lower coverage
    values = c(values, sum(item$true > unlist(lower[j])) / nrow(item))
    
    # MIS (2L)
    #values = c(values, sum(unlist(unlist(upper[j])) - unlist(lower[j]) + (2/alpha) * ((item$true < unlist(lower[j])) * (unlist(lower[j]) - item$true) + (item$true > unlist(unlist(upper[j]))) * (item$true - unlist(unlist(upper[j]))))) / nrow(item))
    values = c(values, sum(((unlist(upper[j]) - unlist(lower[j]))/item$mod_CLV) + (2/alpha) * ((item$true < unlist(lower[j])) * ((unlist(lower[j]) - item$true)/item$mod_CLV) + (item$true > unlist(unlist(upper[j]))) * ((item$true - unlist(unlist(upper[j])))/item$mod_CLV)) / nrow(item)))
    
    # Bias (2L)
     values = c(values, sum(item$true - item$mod_CLV) / (mean(item$true) * nrow(item)))
    
    # Interval score (20PI)
    #values = c(values, sum(-2 * alpha * (unlist(unlist(upper[j])) - unlist(lower[j])) - 4 * ((item$true < unlist(lower[j])) * (unlist(lower[j]) - item$true) + (item$true > unlist(unlist(upper[j]))) * (item$true - unlist(unlist(upper[j]))))) / nrow(item))
    
    # Mean scaled prediction interval width (comparable with NMPIL and PINAW but scaled with mean and not with standard deviation)
    mspiw = data.table("width" = (unlist(unlist(upper[j])) - unlist(lower[j])) / item$mod_CLV,
                        "CLV" = item$mod_CLV,
                        "weight" = 0)
    
    values = c(values, mean(mspiw$width))
    
    # possibility to assign higher weights to more important customers
       mspiw$weight = mspiw$CLV / sum(mspiw$CLV)
       values = c(values, sum(mspiw$width * mspiw$weight))
    
    # (Sharpness-Width Ratio)
    values = c(values, unlist(PICP / mean((unlist(upper[j]) - unlist(lower[j])) / item$mod_CLV))) # coverage per width

    measures_CET = cbind(measures_CET, round(unlist(values),4))
    names(measures_CET)[length(names(measures_CET))] = paste(names(items)[counter], "-", methods[j])
    # print(paste(x, "------------------------------"))
    # print(rownames)
    rownames_CET = append(rownames_CET, paste(names(items)[counter], "-", methods[j]))
  }
}

quantiles = quantile(benchmark_PTS$mod_CLV, c(1/3,2/3))
s1 = benchmark_PTS[mod_CLV < quantiles[1]]
s2 = benchmark_PTS[mod_CLV >= quantiles[1] & mod_CLV < quantiles[2]]
s3 = benchmark_PTS[mod_CLV >= quantiles[2]]
items = list(s1 = s1, s2 = s2, s3 = s3)
counter = 0
methods = list("BS", "M1", "CP", "QR")
rownames = list()
alpha = 0.05

# lower = list(item$mod_PTS_PI_05, item$PB_PTS_PI_05, item$CP_PTS_PI_05)
# upper = list(item$mod_PTS_PI_95, item$PB_PTS_PI_95, item$CP_PTS_PI_95)

for (item in items){
  counter = counter + 1
  lower = list(item$mod_PTS_PI_05, item$PB_PTS_PI_05, item$CP_PTS_PI_05, item$QR_PTS_PI_05)
  upper = list(item$mod_PTS_PI_95, item$PB_PTS_PI_95, item$CP_PTS_PI_95, item$QR_PTS_PI_95)
  
  for (j in 1:4){
    values = list()
    
    # Coverage/PICP (16PI, 20PI)
    PICP = sum(item$true < unlist(upper[j]) & item$true > unlist(lower[j])) / nrow(item)
    values = c(values, PICP)
    
    # Average coverage error (16PI, 20PI)
    values = c(values, 0.90 - PICP)
    
    # Upper coverage (2L)
    values = c(values, sum(item$true < unlist(unlist(upper[j]))) / nrow(item))
    
    # Lower coverage
    values = c(values, sum(item$true > unlist(lower[j])) / nrow(item))
    
    # MIS (2L)
    #values = c(values, sum(unlist(unlist(upper[j])) - unlist(lower[j]) + (2/alpha) * ((item$true < unlist(lower[j])) * (unlist(lower[j]) - item$true) + (item$true > unlist(unlist(upper[j]))) * (item$true - unlist(unlist(upper[j]))))) / nrow(item))
    values = c(values, sum(((unlist(upper[j]) - unlist(lower[j]))/item$mod_CLV) + (2/alpha) * ((item$true < unlist(lower[j])) * ((unlist(lower[j]) - item$true)/item$mod_CLV) + (item$true > unlist(unlist(upper[j]))) * ((item$true - unlist(unlist(upper[j])))/item$mod_CLV)) / nrow(item)))
    
    # Bias (2L)
    values = c(values, sum(item$true - item$mod_CLV) / (mean(item$true) * nrow(item)))
    
    # Interval score (20PI)
    #values = c(values, sum(-2 * alpha * (unlist(unlist(upper[j])) - unlist(lower[j])) - 4 * ((item$true < unlist(lower[j])) * (unlist(lower[j]) - item$true) + (item$true > unlist(unlist(upper[j]))) * (item$true - unlist(unlist(upper[j]))))) / nrow(item))
    
    # Mean scaled prediction interval width (comparable with NMPIL and PINAW but scaled with mean and not with standard deviation)
    mspiw = data.table("width" = (unlist(unlist(upper[j])) - unlist(lower[j])) / item$mod_CLV,
                       "CLV" = item$mod_CLV,
                       "weight" = 0)
    
    values = c(values, mean(mspiw$width))
    
    # possibility to assign higher weights to more important customers
    mspiw$weight = mspiw$CLV / sum(mspiw$CLV)
    values = c(values, sum(mspiw$width * mspiw$weight))
    
    # (Sharpness-Width Ratio)
    values = c(values, unlist(PICP / mean((unlist(upper[j]) - unlist(lower[j])) / item$mod_CLV))) # coverage per width
    
    measures_PTS = cbind(measures_PTS, round(unlist(values),4))
    names(measures_PTS)[length(names(measures_PTS))] = paste(names(items)[counter], "-", methods[j])
    rownames = append(rownames, paste(names(items)[counter], "-", methods[j]))
  }
}

# Showing the measures tables
measures_CET = transpose(measures_CET)
colnames(measures_CET) = unlist(measures_CET[1,])
measures_CET = measures_CET[-1,]
measures_CET = data.table("run" = unlist(rownames_CET), measures_CET)
measures_CET

measures_PTS = transpose(measures_PTS)
colnames(measures_PTS) = unlist(measures_PTS[1,])
measures_PTS = measures_PTS[-1,]
measures_PTS = data.table("run" = unlist(rownames), measures_PTS)
measures_PTS
# 
# # Plotting the mspiw plot
# plot_data_mspiw = data.table("lower_BS" = results_boots$predicted.CLV.CI.5,
#                              "upper_BS" = results_boots$predicted.CLV.CI.95,
#                              "lower_M1" = intervals_MB$`MB_CLV_05%`,
#                              "upper_M1" = intervals_MB$`MB_CLV_95%`,
#                              "lower_M2" = intervals_PB$`PB_CLV_05%`,
#                              "upper_M2" = intervals_PB$`PB_CLV_95%`,
#                              "lower_CP" = intervals_CP$Lower,
#                              "upper_CP" = intervals_CP$Upper,
#                              "CLV" = results_boots$predicted.CLV)
# 
# plot_data_mspiw$width_BS = (plot_data_mspiw$upper_BS - plot_data_mspiw$lower_BS) / results_boots$predicted.CLV
# plot_data_mspiw$width_M1 = (plot_data_mspiw$upper_M1 - plot_data_mspiw$lower_M1) / results_boots$predicted.CLV
# plot_data_mspiw$width_M2 = (plot_data_mspiw$upper_M2 - plot_data_mspiw$lower_M2) / results_boots$predicted.CLV
# plot_data_mspiw$width_CP = (plot_data_mspiw$upper_CP - plot_data_mspiw$lower_CP) / results_boots$predicted.CLV
# 
# ggplot(plot_data_mspiw[CLV < 1000,], aes(x = CLV)) + 
#   geom_line(aes(y = width_BS, color = "blue")) +
#   geom_line(aes(y = width_M1, color = "red")) +
#   geom_line(aes(y = width_M2, color = "green")) +
#   geom_line(aes(y = width_CP, color = "black")) +
#   labs(title = "Absolute interval width/CLV for different CLV values and intervals methods", y = "Relative interval width") +
#   scale_color_identity(name = "Model",
#                        breaks = c("blue", "red", "green", "black"),
#                        labels = c("Bootstrap", "Method 1", "Method 2", "CP"),
#                        guide = "legend")
# 
# # Plotting the relative distribution plot
# 
# rel_data_CP = data.table("Id" = results_boots$Id,
#                       "prediction" = results_boots$predicted.CLV,
#                       "lower_PI" = plot_data_mspiw$lower_CP,
#                       "upper_PI" = plot_data_mspiw$upper_CP,
#                       "true" = benchmark_data$true,
#                       "position" = 0)
# 
# rel_data_M1 = data.table("Id" = results_boots$Id,
#                          "prediction" = results_boots$predicted.CLV,
#                          "lower_PI" = plot_data_mspiw$lower_M1,
#                          "upper_PI" = plot_data_mspiw$upper_M1,
#                          "true" = benchmark_data$true,
#                          "position" = 0)
# 
# rel_data_CP$position = (rel_data_CP$true - rel_data_CP$lower_PI) / (rel_data_CP$upper_PI - rel_data_CP$lower_PI)
# rel_data_M1$position = (rel_data_M1$true - rel_data_M1$lower_PI) / (rel_data_M1$upper_PI - rel_data_M1$lower_PI)
# 
# ggplot(rel_data_CP, aes(x = position)) +
#   geom_histogram(binwidth = 0.05) +
#   geom_vline(xintercept = 0) +
#   geom_vline(xintercept = 1) +
#   labs(title = "Distribution of true observations relative to their PIs",
#        x = "0: On the lower boundary, 1: On the upper boundary, outside: Outside of the PIs")
# 
# ggplot(rel_data_M1, aes(x = position)) +
#   geom_histogram(binwidth = 0.05) +
#   geom_vline(xintercept = 0) +
#   geom_vline(xintercept = 1) +
#   labs(title = "Distribution of true observations relative to their PIs",
#        x = "0: On the lower boundary, 1: On the upper boundary, outside: Outside of the PIs")

