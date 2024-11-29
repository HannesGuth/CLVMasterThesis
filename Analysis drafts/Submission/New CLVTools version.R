library(CLVTools)
library(data.table)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load data
source(paste0(getwd(), "/Data preparation.r"))

############ SELECT THE DATA SET HERE
data_list = gift_list # does not worl for me
data_list = el_list # works for me 
data_list = multi_list # does not work for me
data_list = apparel_list # does not work for me

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
results_boots = predict(est.data2, uncertainty="boots", prediction.end = end2, num.boots = 100)
