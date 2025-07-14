install.packages("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/CLVTools", repos = NULL, type="source")

# Load package
library(CLVTools)

# Load data
data("apparelTrans")
clv.apparel <- clvdata(apparelTrans,  
                       date.format="ymd", 
                       time.unit = "week",
                       estimation.split = 40,
                       name.id = "Id",
                       name.date = "Date",
                       name.price = "Price")

# Estimate standard Pareto/NBD Model
est.pnbd <- pnbd(clv.data = clv.apparel, verbose = TRUE)
summary(est.pnbd)
results <- predict(est.pnbd, predict.spending = FALSE)
est.gg <- gg(clv.data = clv.apparel)
print(results)

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

# Evaluation
# CLV = DERT * predicted mean spending
install.packages("ggplot2")
alive = data.table(ID = results$Id,
                   PAlive = floor(results$PAlive * 10)/10,
                   Predicted = results$PAlive > 0.5,
                   True = apparelTrans[, max(Date), by = Id]$V1 > apparelTrans[, min(Date) + 50*7, by = Id]$V1
)
alive

x = alive[, mean(True), by = PAlive]
x = x[order(x$PAlive),]
x
ggplot(x, aes(x = PAlive, y = V1)) +
  geom_line(color = "black") +
  labs(x = "Predicted", y = "True", title = "Predicted PAlive and True PAlive")

x = apparelTrans[apparelTrans$Date>min(apparelTrans$Date),]
x$Year = year(x$Date)
x$Week = week(x$Date)
x$YearWeek = paste(x$Year, "-", x$Week)
x[, Test := sum(Price>0), by = YearWeek]
x = x[order(c(x$Date)),]
x
y = x[,c("Week", "Test")]
a = y[!duplicated(y)]
a$index = list(1:81)
a
ggplot(a, aes(x = index, y = Test)) +
  geom_line()
plot(est.pnbd)

# Repeat transactions
rt = fread("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Gamma distribution/Test.csv")

p = 6.24983547654959
q = 3.7441106896737
gamma = 15.4423198312514
ez = p*gamma/(q-1)

rt$weight = (q-1)/(p*rt$x+q-1)
rt$ezxzbar = rt$weight*ez+(1-rt$weight)*rt$zbar
rt

#
wo = apparelTrans[apparelTrans$Date>min(apparelTrans$Date),]
wo = wo[, mean(Price), by = Id]
x = apparelTrans

p = 2.305
q = 17.148
gamma = 279.974
ez = p*gamma/(q-1)

rtid = data.table("Id" = x[, .N, by = Id]$Id)

rtid$x = x[, .N, by = Id]$N - 1
#rtid$zbar = (rtid$x>1) * x[, mean(Price), by = Id]$V1
rtid = merge(x = rtid, y = wo[,c("Id","V1")], by = "Id", all = TRUE)
setnames(rtid, "V1", "zbar")
rtid[is.na(rtid)] <- 0
rtid$weight = (q-1)/(p*rtid$x+q-1)
rtid$ezxzbar = rtid$weight*ez+(1-rtid$weight)*rtid$zbar
rtid

# Modifying parameters
est.gg@prediction.params.model[1] = 1
est.pnbd




