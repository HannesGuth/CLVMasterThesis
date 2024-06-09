library(CLVTools)
library(data.table)
library(compiler)
library(ggplot2)
library(profvis)
library(rockchalk)
library(doParallel)

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

source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Coefficients.r")
source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Bayesian approach.r")
source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Quantile regression.r")
source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Conformal prediction averaging std.r")
source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Plotting.r")
source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Benchmarking Function 3 Methods.r")



