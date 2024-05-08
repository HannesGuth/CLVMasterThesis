install.packages("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/CLVTools", repos = NULL, type="source")

# Load package
library(CLVTools)
library(data.table)
library(compiler)
library(ggplot2)
library(profvis)
library(rockchalk)

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
results <- predict(est.pnbd, predict.spending = TRUE, continuous.discount.factor = 0.1)
est.gg <- gg(clv.data = clv.apparel)

# Boostrapping to get prediction intervalls
set.seed(1)
results_boots <- predict(est.pnbd, uncertainty="boots")


# CLV
clv = data.table("old" = apparelTrans[Date < min(Date) + splitWeek * 7, sum(Price), by = Id],
                 "new" = 0,
                 "sum" = 0)

new = apparelTrans
new$Price = (new$Date >= min(new$Date) + splitWeek * 7) * new$Price
new$weeks = as.numeric(difftime(new$Date, as.Date("2005-01-03") + splitWeek * 7, units = "weeks"))
new$DiscountedPrice = new$Price / ((1 + 0.1)^new$weeks)

clv$new = new[, sum(DiscountedPrice), by = Id]$V1
clv$sum = clv$old.V1 + clv$new
clv$lower = results_boots$predicted.CLV.CI.5
clv$upper = results_boots$predicted.CLV.CI.95
clv

sum(clv$new < clv$upper & clv$new > clv$lower)

x = new[Id == 100,]
sum(x$DiscountedPrice)

# From (1L)
new2 = apparelTrans
new2 = new2[duplicated(new2$Id),]
new2$weeks = as.numeric(difftime(new2$Date, as.Date("2005-01-03"), units = "weeks"))
new2$DiscountedPrice = new2$Price/((1+log(1+0.1))^new2$weeks)
new2 = merge(x = results[, c("Id", "predicted.CLV")], new2[, sum(DiscountedPrice), by = Id], by = "Id", all.x = TRUE)
colnames(new2) = c("Id", "predicted.CLV", "allwithfirst (1L)")
#new2 = merge(x = new2, y = results[,c("Id","predicted.CLV")], by = "Id", all.y = TRUE)
new2$allwithoutfirst = new2$allwithfirst - apparelTrans[!duplicated(apparelTrans$Id),]$Price
intermediate = apparelTrans
intermediate = intermediate[Date > as.Date("2005-01-03") + splitWeek * 7,]
intermediate$weeks = as.numeric(difftime(intermediate$Date, as.Date("2005-01-03"), units = "weeks"))
intermediate$DiscountedPrice = intermediate$Price/((1+log(1+0.1/52))^intermediate$weeks)
intermediate = intermediate[, sum(DiscountedPrice), by = "Id"]
new2 = merge(x = new2, y = intermediate, by = "Id", all.x = TRUE)
colnames(new2) = c("Id", "predicted.CLV", "allwithfirst (1L)", "allwithoutfirst", "onlyholdout")
new2

# With model definition
new3 = data.table("Id" = results$Id,
                  "actual.x" = results$actual.x,
                  "actual.total.spending" = results$actual.total.spending,
                  "CET" = results$CET,
                  "recent" = 0,
                  "DERT_old" = results$DERT,
                  "CLV_old" = results$predicted.CLV)

new3$DERT_new = new3$actual.x / 3.781462
new3$mean_spending = ifelse(new3$actual.x == 0, 0, new3$actual.total.spending/new3$actual.x)
new3$CLV = new3$DERT_new * new3$mean_spending

# Calculate DERT with the BTYD package
  cutoffSplitweek = apparelTrans[Date <= as.Date("2005-01-03") + splitWeek * 7,]
  new3$recent = as.numeric(difftime(cutoffSplitweek[, max(Date), by = "Id"]$V1, as.Date("2005-01-03"), units = "weeks"))
  rptr = cutoffSplitweek[duplicated(cutoffSplitweek$Id),]
  rptr = rptr[,.N, by = "Id"]
  rptr = merge(x = results, y = rptr, by = "Id", all.x = TRUE)
  rptr[is.na(rptr$N)] = 0
  params = c(0.7858, 5.3312, 0.3606, 11.8221)
  pnbd.DERT(params, cutoffSplitweek[,.N, by = "Id"]$N, new3$recent, 39.85714, 0.1/52, hardie = FALSE)
  
  dtdert = data.table("BTYD" = pnbd.DERT(params, rptr$N, new3$recent, 40, 0.1),
                      "CLVTools" = results$DERT,
                      "recent" = new3$recent)
  View(dtdert)
