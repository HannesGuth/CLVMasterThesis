library(CLVTools)
library(data.table)
library(compiler)
library(ggplot2)
library(profvis)
library(rockchalk)
library(doParallel)

splitweek = 80 # 130 for gift2
# Load data

#source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Data preparation.r")
clv.gift2 <- clvdata(gift2,  
                     date.format="ymd", 
                     time.unit = "week",
                     estimation.split = splitweek,
                     name.id = "Id",
                     name.date = "Date",
                     name.price = "Price")

# Estimate standard Pareto/NBD Model
est.gift2 <- pnbd(clv.data = clv.gift2, verbose = TRUE)
results_general <- predict(est.gift2, predict.spending = TRUE)
print(results_general)


source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Bootstrapping.r")
source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Coefficients.r")
source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Bayesian approach.r")
source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Quantile regression managerial gift.r")
source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Conformal prediction managerial gift.r")
source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Benchmarking.r")

source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Plotting.r")
source("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Drafts/Analysis drafts/Draft Benchmarking Function 3 Methods.r")

  
  
  
  
  
  
  
  
  






