# DESCRIPTION

# Not used in this work

################################################################

library(CLVTools)
library(data.table)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load data
# source(paste0(getwd(), "/Data preparation.r"))
# data = list(gift_list = gift_list, el_list = el_list, multi_list = multi_list, apparel_list = apparel_list)
# saveRDS(data, file = paste0(getwd(), "/VersionTestingData.RData"))
data = readRDS(paste0(getwd(), "/VersionTestingData.RData"))
############ SELECT THE DATA SET HERE
data_list = data$gift_list # does not work for me
data_list = data$el_list # works for me
data_list = data$multi_list # does not work for me
data_list = data$apparel_list # does not work for me

set.seed(1)
# Setting the periods
data2 = data_list$data2
splitweek2 = data_list$l2
end2 = data_list$p2
  
clv.data2 = clvdata(data2,
                     date.format="ymd",
                     time.unit = "week",
                     estimation.split = splitweek2,
                     name.id = "Id",
                     name.date = "Date",
                     name.price = "Price")

# Estimate standard Pareto/NBD Model
est.data2 = pnbd(clv.data = clv.data2, verbose = TRUE)

# Bootstrapping to get prediction intervals
results_boots = predict(est.data2, uncertainty="boots", num.boots = 100)
