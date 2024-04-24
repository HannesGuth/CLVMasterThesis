install.packages("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/CLVTools", repos = NULL, type="source")

# Load package
library(CLVTools)
library(data.table)
library(compiler)
library(ggplot2)
library(profvis)
library(rockchalk)

splitWeek = 20

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
results <- predict(est.pnbd, predict.spending = TRUE, continuous.discount.factor = 0.1)
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
new2$weeks = as.numeric(difftime(new2$Date, as.Date("2005-01-03"), units = "weeks"))
new2$DiscountedPrice = new2$Price/((1+log(1+0.1/52))^new2$weeks)
new2[, sum(DiscountedPrice), by = Id]

# With model definition
new3 = data.table("Id" = results$Id,
                  "actual.x" = results$actual.x,
                  "actual.total.spending" = results$actual.total.spending,
                  "CET" = results$CET,
                  "DERT_old" = results$DERT,
                  "CLV_old" = results$predicted.CLV)

new3$DERT_new = new3$actual.x / 3.781462
new3$mean_spending = ifelse(new3$actual.x == 0, 0, new3$actual.total.spending/new3$actual.x)
new3$CLV = new3$DERT_new * new3$mean_spending



