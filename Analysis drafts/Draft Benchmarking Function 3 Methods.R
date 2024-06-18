# Benchmarking

benchmark_CET = data.table("Id" = results_boots$Id,
                           "mod_CET_PI_05" = intervals_BS$CET_lower,
                           "mod_CET_PI_95" = intervals_BS$CET_upper,
                           "PB_CET_PI_05" = intervals_PB$PB_CET_05,
                           "PB_CET_PI_95" = intervals_PB$PB_CET_95,
                           "CP_CET_PI_05" = intervals_CP_m$CET_Lower,
                           "CP_CET_PI_95" = intervals_CP_m$CET_Upper,
                           "BA_CET_PI_05" = intervals_BA$Lower,
                           "BA_CET_PI_95" = intervals_BA$Upper,
                           "QR_CET_PI_05" = intervals_QR_m$CET_lower,
                           "QR_CET_PI_95" = intervals_QR_m$CET_upper,
                           "pred" = results$CET,
                           "true" = results$actual.x,
                           "mod_CLV" = results$predicted.CLV
)

benchmark_PTS = data.table("Id" = results_boots$Id,
                           "mod_PTS_PI_05" = 0,
                           "mod_PTS_PI_95" = 0,
                           "PB_PTS_PI_05" = intervals_PB$PB_PTS_05,
                           "PB_PTS_PI_95" = intervals_PB$PB_PTS_95,
                           "CP_PTS_PI_05" = intervals_CP_m$PTS_Lower,
                           "CP_PTS_PI_95" = intervals_CP_m$PTS_Upper,
                           "QR_PTS_PI_05" = intervals_QR_m$PTS_lower,
                           "QR_PTS_PI_95" = intervals_QR_m$CET_upper,
                           "pred" = results$predicted.total.spending,
                           "true" = results$actual.total.spending,
                           "mod_CLV" = results$predicted.CLV
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

