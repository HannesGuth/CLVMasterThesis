# Load package
library(CLVTools)
library(data.table)
library(compiler)
library(ggplot2)
library(profvis)
library(rockchalk)
library(bit64)

# Load data
splitWeek = 40
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

r = est.pnbd@prediction.params.model[1]
a = est.pnbd@prediction.params.model[2]
s = est.pnbd@prediction.params.model[3]
b = est.pnbd@prediction.params.model[4]
t = splitWeek
ext = ((r*b) / (a*(s-1))) * (1 - ((b/(1+b))^(s-1)))
ext
ext2 = ext + ((2*r*(r+1)*b)/(a*a*(s-1))) * (b/(s-2) - (b/(s-2))*(b/(b+t))^(s-2) - t*(b/(b+t))^(s-1))

# Split customers
low = results[predicted.CLV < 35,]
mid = results[predicted.CLV >= 35 & predicted.CLV < 70,]
high = results[predicted.CLV >= 70,]

high = as.vector(unlist(high$Id))
high = apparelTrans[Id %in% high,]
clv.high = clvdata(high,
                  date.format="ymd", 
                  time.unit = "week",
                  estimation.split = splitWeek,
                  name.id = "Id",
                  name.date = "Date",
                  name.price = "Price")
est.high = pnbd(clv.data = clv.high)


r = as.numeric(est.high@prediction.params.model[1])
a = as.numeric(est.high@prediction.params.model[2])
s = as.numeric(est.high@prediction.params.model[3])
b = as.numeric(est.high@prediction.params.model[4])
t = splitWeek
ext = ((r*b) / (a*(s-1))) * (1 - ((b/(1+b))^(s-1)))
ext
ext2 = ext + ((2*r*(r+1)*b)/(a*a*(s-1))) * (b/(s-2) - (b/(s-2))*(b/(b+t))^(s-2) - t*(b/(b+t))^(s-1))
sqrt(ext2 - ext^2)



est.pnbd <- pnbd(clv.data = clv.apparel, verbose = TRUE)
summary(est.pnbd)
est.ggomnbd = ggomnbd(clv.data = clv.apparel, verbose = TRUE)
predict(est.ggomnbd)
est.bgnbd = bgnbd(clv.data = clv.apparel, verbose = TRUE)
predict(est.bgnbd)
results <- predict(est.pnbd, predict.spending = TRUE)
print(results)
est.gg <- gg(clv.data = clv.apparel)
predict(est.gg)






