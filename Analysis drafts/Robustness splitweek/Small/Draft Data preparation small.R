# Electronics

electronics = fread("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Datasets/Electronics retailer/durdata1_final.csv")
electronics = electronics[, c("HOUSEHOLD_ID", "TRANSACTION_NBR", "TRANSACTION_TOTAL", "TRANSACTION_DATE", "QUANTITY", "UNIT_PRICE", "EXTENDED_PRICE")]
Sys.setlocale("LC_TIME", "English")
electronics$Date = as.Date(electronics$TRANSACTION_DATE, format = "%d%b%Y")
electronics$Price = electronics$QUANTITY * electronics$UNIT_PRICE
elclv = electronics[, c("HOUSEHOLD_ID", "Date", "Price")]
colnames(elclv) = c("Id", "Date", "Price")
elclv = elclv[complete.cases(elclv)]
x = elclv[, min(Date), by = "Id"]
el1 = x[V1 %in% "2001-01-06"]
el2 = x[V1 %in% "2002-01-05"]
x = x[, .N, by = "V1"]
x[order(N, decreasing = TRUE)]
el1 = merge(x = el1, y = elclv, by = "Id", all.x = TRUE)
el2 = merge(x = el2, y = elclv, by = "Id", all.x = TRUE)
el1 = el1[,-"V1"]
el2 = el2[,-"V1"]
el1$Id = as.character(el1$Id)
el2$Id = as.character(el2$Id)

clv.el1 <- clvdata(el1,
                   date.format="ymd", 
                   time.unit = "week",
                   estimation.split = 40, # 10,20,50,80,120
                   name.id = "Id",
                   name.date = "Date",
                   name.price = "Price")
mod.el1 = pnbd(clv.data = clv.el1)
results.el1 = predict(mod.el1, predict.spending = TRUE, prediction.end = "2002-01-05")

clv.el2 <- clvdata(el2,  
                  date.format="ymd", 
                  time.unit = "week",
                  estimation.split = 140, # 15,50,80,140
                  name.id = "Id",
                  name.date = "Date",
                  name.price = "Price")
mod.el2 = pnbd(clv.data = clv.el2)
results.el2 = predict(mod.el2, predict.spending = TRUE)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Gifts

gift = fread("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Datasets/Gift shop/gift.csv")
gift = gift[, c("ID", "date", "amount")]
colnames(gift) = c("Id", "Date", "Price")
x = gift[, min(Date), by = "Id"]
x = x[, .N, by = "V1"]
x[order(-x$N),]
x = gift[, min(Date), by = "Id"]

# gift2
gift2 = x[V1 %in% "2002-12-16"]
gift2 = merge(x = gift, y = gift2, by = "Id")
gift2 = gift2[, -4]
gift2$Date = as.Date(gift2$Date, format = "%d%b%Y")

# gift1
gift1 = x[V1 %in% "2001-12-11"]
gift1 = merge(x = gift, y = gift1, by = "Id")
gift1 = gift1[, -4]
gift1$Id = as.character(gift1$Id)
gift1$Date = as.Date(gift1$Date, format = "%d%b%Y")

# gift2
clv.gift2 <- clvdata(gift2,  
                       date.format="ymd", 
                       time.unit = "week",
                       estimation.split = 130,
                       name.id = "Id",
                       name.date = "Date",
                       name.price = "Price")
est.gift2 = pnbd(clv.data = clv.gift2)
results.gift2 = predict(est.gift2, predict.spending = TRUE)

# gift1
clv.gift1 <- clvdata(gift1,  
                     date.format="ymd", 
                     time.unit = "week",
                     estimation.split = 130,
                     name.id = "Id",
                     name.date = "Date",
                     name.price = "Price")
est.gift1 = pnbd(clv.data = clv.gift1)
results.gift1 = predict(est.gift1, predict.spending = TRUE)
