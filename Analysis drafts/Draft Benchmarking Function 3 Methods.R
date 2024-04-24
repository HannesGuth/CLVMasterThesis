install.packages("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/CLVTools", repos = NULL, type="source")

# Load package
library(CLVTools)
library(data.table)
library(compiler)
library(ggplot2)
library(profvis)
library(rockchalk)
library(TAF)

splitWeek = 40

# Load data
data("apparelTrans")
clv.apparel <- clvdata(apparelTrans,  
                       date.format="ymd", 
                       time.unit = "week",
                       estimation.split = splitWeek,
                       name.id = "Id",
                       name.date = "Date",
                       name.price = "Price")

# Estimate standard Pareto/NBD Model
est.pnbd <- pnbd(clv.data = clv.apparel, verbose = TRUE)
summary(est.pnbd)
results <- predict(est.pnbd, predict.spending = TRUE)
print(results)
est.gg <- gg(clv.data = clv.apparel)
predict(est.gg)

# Boostrapping to get prediction intervalls
set.seed(1)
results_boots <- predict(est.pnbd, uncertainty="boots")

# A more detailed look at the results
results_boots$predicted.CLV
results_boots$predicted.CLV.CI.5
results_boots$predicted.CLV.CI.95
results_boots$actual.total.spending
hist((results_boots$predicted.CLV-results_boots$predicted.CLV.CI.5)/results_boots$predicted.CLV*100)
hist((results_boots$predicted.CLV.CI.95-results_boots$predicted.CLV)/results_boots$predicted.CLV*100)

# Benchmarking

benchmark_data = data.table("Id" = results_boots$Id,
                           "mod_PI_05" = results_boots$predicted.CLV.CI.5,
                           "mod_PI_95" = results_boots$predicted.CLV.CI.95,
                           "PB_PI_05" = intervals_PB$`PB_CLV_05%`,
                           "PB_PI_95" = intervals_PB$`PB_CLV_95%`,
                           "MB_PI_05" = intervals_MB$`MB_CLV_05%`,
                           "MB_PI_95" = intervals_MB$`MB_CLV_95%`,
                           "mod_CLV" = results_boots$predicted.CLV
)

benchmark_data$true = benchmark_data$mod_CLV + (benchmark_data$mod_CLV * runif(nrow(benchmark_data), -0.2, 0.2))
#benchmark_data$true = new3$CLV

measures = data.table("Measure" = c("PICP", "ACE", "Upper coverage", "Lower coverage", "MIS", "Bias", "MSPIW", "MSPIWW", "SWR"))
quantiles = quantile(benchmark_data$mod_CLV, c(0.4,0.7))
s40 = benchmark_data[mod_CLV < quantiles[1]]
s70 = benchmark_data[mod_CLV >= quantiles[1] & mod_CLV < quantiles[2]]
a70 = benchmark_data[mod_CLV >= quantiles[2]]
items = list(s40 = s40, s70 = s70, a70 = a70)
counter = 0
methods = list("BS", "M1", "M2")
alpha = 0.05

for (item in items){
  counter = counter + 1
  lower = list(item$mod_PI_05, item$PB_PI_05, item$MB_PI_05)
  upper = list(item$mod_PI_95, item$PB_PI_95, item$MB_PI_95)

  for (j in 1:3){
    values = list()
    
    # Coverage/PICP (16PI, 20PI)
    PICP = sum(item$true < unlist(unlist(upper[j])) & item$true > unlist(lower[j])) / nrow(item)
    values = c(values, PICP)
    
    # Average coverage error (16PI, 20PI)
    values = c(values, 0.95 - PICP)

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
    
    # Mean scaled prediction interval width (comparable with NMPIL but scaled with mean and not with standard deviation)
    mspiw = data.table("width" = (unlist(unlist(upper[j])) - unlist(lower[j])) / item$mod_CLV,
                        "CLV" = item$mod_CLV,
                        "weight" = 0)
    values = c(values, mean(mspiw$width))
    
    # possibility to assign higher weights to more important customers
       mspiw$weight = mspiw$CLV / sum(mspiw$CLV)
       values = c(values, sum(mspiw$width * mspiw$weight))
    
    # (Sharpness-Width Ratio)
    values = c(values, unlist(PICP / mean((unlist(upper[j]) - unlist(lower[j])) / item$mod_CLV))) # coverage per width

    measures = cbind(measures, round(unlist(values),4))
    names(measures)[length(names(measures))] = paste(names(items)[counter], "-", methods[j])
  }
}

# Plotting the mspiw plot

plot_data_mspiw = data.table("lower_BS" = results_boots$predicted.CLV.CI.5,
                             "upper_BS" = results_boots$predicted.CLV.CI.95,
                             "lower_M1" = intervals_MB$`MB_CLV_05%`,
                             "upper_M1" = intervals_MB$`MB_CLV_95%`,
                             "lower_M2" = intervals_PB$`PB_CLV_05%`,
                             "upper_M2" = intervals_PB$`PB_CLV_95%`,
                             "CLV" = results_boots$predicted.CLV)

plot_data_mspiw$width_BS = (plot_data_mspiw$upper_BS - plot_data_mspiw$lower_BS) / results_boots$predicted.CLV
plot_data_mspiw$width_M1 = (plot_data_mspiw$upper_M1 - plot_data_mspiw$lower_M1) / results_boots$predicted.CLV
plot_data_mspiw$width_M2 = (plot_data_mspiw$upper_M2 - plot_data_mspiw$lower_M2) / results_boots$predicted.CLV

ggplot(plot_data_mspiw[CLV < 1000,], aes(x = CLV)) + 
  geom_line(aes(y = width_BS, color = "blue")) +
  geom_line(aes(y = width_M1, color = "red")) +
  geom_line(aes(y = width_M2, color = "green")) +
  labs(title = "Absolute interval width/CLV for different CLV values and intervals methods", y = "Relative interval width") +
  scale_color_identity(name = "Model",
                       breaks = c("blue", "red", "green"),
                       labels = c("Bootstrap", "Method 1", "Method 2"),
                       guide = "legend")

# Plotting the relative distribution plot

rel_data = data.table("Id" = results_boots$Id,
                      "prediction" = results_boots$predicted.CLV,
                      "lower_PI" = lower,
                      "upper_PI" = upper,
                      "true" = benchmark_data$true,
                      "position" = 0)

rel_data$position = (rel_data$true - rel_data$lower_PI) / (rel_data$upper_PI - rel_data$lower_PI)

ggplot(rel_data, aes(x = position)) +
  geom_histogram(binwidth = 0.1) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 1) +
  labs(title = "Distribution of true observations relative to their PIs",
       x = "0: On the lower boundary, 1: On the upper boundary, outside: Outside of the PIs")
