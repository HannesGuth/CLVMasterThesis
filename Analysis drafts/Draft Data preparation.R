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
el1 = x[V1 >= "2000-01-01" & V1 <= "2000-03-31"]
el2 = x[V1 >= "2002-01-01" & V1 <= "2002-03-31"]
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
                   estimation.split = 52, # 10,20,50,80,120
                   name.id = "Id",
                   name.date = "Date",
                   name.price = "Price")
mod.el1 = pnbd(clv.data = clv.el1)
results.el1 = predict(mod.el1, predict.spending = TRUE)

clv.el2 <- clvdata(el2,  
                  date.format="ymd", 
                  time.unit = "week",
                  estimation.split = 52, # 15,50,80,140
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
gift2 = x[V1 >= "2004-12-08" & V1 <= "2004-12-15"]
gift2 = merge(x = gift, y = gift2, by = "Id")
gift2 = gift2[, -4]
gift2$Date = as.Date(gift2$Date, format = "%d%b%Y")

# gift1
gift1 = x[V1 >= "2002-12-08" & V1 <= "2002-12-15"]
gift1 = merge(x = gift, y = gift1, by = "Id")
gift1 = gift1[, -4]
gift1$Id = as.character(gift1$Id)
gift1$Date = as.Date(gift1$Date, format = "%d%b%Y")

# gift2
clv.gift2 <- clvdata(gift2,  
                       date.format="ymd", 
                       time.unit = "week",
                       estimation.split = 120,
                       name.id = "Id",
                       name.date = "Date",
                       name.price = "Price")
est.gift2 = pnbd(clv.data = clv.gift2)
results.gift2 = predict(est.gift2, predict.spending = TRUE)

# gift1
clv.gift1 <- clvdata(gift1,  
                     date.format="ymd", 
                     time.unit = "week",
                     estimation.split = 52,
                     name.id = "Id",
                     name.date = "Date",
                     name.price = "Price")
est.gift1 = pnbd(clv.data = clv.gift1)
results.gift1 = predict(est.gift1, predict.spending = TRUE)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Multichannel
multi = fread("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Datasets/Multichannel/special.csv")
multi = multi[,c("CUSTNO","ORDER_DATE","EXT_PRICE","QUANTITY")]
multi$EXT_PRICE = multi$EXT_PRICE * multi$QUANTITY
multi = multi[,-4]
colnames(multi) = c("Id","Date","Price")
multi$Id = as.character(multi$Id)
multi$Date = as.Date(multi$Date, format = "%d.%m.%Y")
x = multi[, min(Date), by = "Id"]
multi1 = x[V1 >= "2005-12-01" & V1 <= "2005-12-31"]
multi2 = x[V1 >= "2008-12-01" & V1 <= "2008-12-31"]
x = x[, .N, by = "V1"]
x[order(N, decreasing = TRUE)]
multi1 = merge(x = multi1, y = multi, by = "Id", all.x = TRUE)
multi2 = merge(x = multi2, y = multi, by = "Id", all.x = TRUE)
multi1 = multi1[,-"V1"]
multi2 = multi2[,-"V1"]

clv.multi1 <- clvdata(multi1, ################# 60 for split, 100 for end
                   date.format="ymd", 
                   time.unit = "week",
                   estimation.split = 52, # 10,20,50,80,120
                   name.id = "Id",
                   name.date = "Date",
                   name.price = "Price")
mod.multi1 = pnbd(clv.data = clv.multi1)
results.multi1 = predict(mod.multi1, predict.spending = TRUE, prediction.end = 104)

clv.multi2 <- clvdata(multi2,  
                   date.format="ymd", 
                   time.unit = "week",
                   estimation.split = 60, # 15,50,80,140
                   name.id = "Id",
                   name.date = "Date",
                   name.price = "Price")
mod.multi2 = pnbd(clv.data = clv.multi2)
results.multi2 = predict(mod.multi2, predict.spending = TRUE, prediction.end = 104)

###########

# Apparel
apparel = fread("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Datasets/PURCHASES - Apparel (Zitzlsperger)/apparel.txt")
apparel$V2 = as.Date(apparel$V2, format = "%d.%m.%Y")
apparel$V1 = as.character(apparel$V1)
colnames(apparel) = c("Id", "Date", "Price")
x = apparel[, min(Date), by = "Id"]
apparel1 = x[V1 >= "2000-01-01" & V1 <= "2000-01-15"]
apparel2 = x[V1 >= "2003-01-01" & V1 <= "2003-01-15"]
x = x[, .N, by = "V1"]
apparel1 = merge(x = apparel1, y = apparel, by = "Id", all.x = TRUE)
apparel2 = merge(x = apparel2, y = apparel, by = "Id", all.x = TRUE)
apparel1 = apparel1[,-2]
apparel2 = apparel2[,-2]

clv.apparel1 <- clvdata(apparel1,
                        date.format="ymd", 
                        time.unit = "week",
                        estimation.split = 52, # 10,20,50,80,120
                        name.id = "Id",
                        name.date = "Date",
                        name.price = "Price")
mod.apparel1 = pnbd(clv.data = clv.apparel1)
results.apparel1 = predict(mod.apparel1, predict.spending = TRUE, prediction.end = 104)

clv.apparel2 <- clvdata(apparel2,  
                        date.format="ymd", 
                        time.unit = "week",
                        estimation.split = 52, # 15,50,80,140
                        name.id = "Id",
                        name.date = "Date",
                        name.price = "Price")
mod.apparel2 = pnbd(clv.data = clv.apparel2)
results.apparel2 = predict(mod.apparel2, predict.spending = TRUE, prediction.end = 104)

#####

gift_list = list(data1 = gift1, data2 = gift2, l1 = 52, l2 = 52, p1 = 52, p2 = 52, BS = TRUE, name = "gift_results", wp1 = FALSE, wp2 = FALSE, smoothwidth = 2)
el_list = list(data1 = el1, data2 = el2, l1 = 52, l2 = 52, p1 = 52, p2 = 52, BS = TRUE, name = "el_results", wp1 = FALSE, wp2 = FALSE, smoothwidth = 2)
multi_list = list(data1 = multi1, data2 = multi2, l1 = 52, l2 = 60, p1 = 104, p2 = 104, BS = FALSE, name = "multi_results", wp1 = FALSE, wp2 = FALSE, smoothwidth = 1)
apparel_list = list(data1 = apparel1, data2 = apparel2, l1 = 52, l2 = 52, p1 = 104, p2 = 104, BS = TRUE, name = "apparel_results", wp1 = FALSE, wp2 = FALSE, smoothwidth = 3)

data_lists = list(gift_list = gift_list, el_list = el_list, multi_list = multi_list, apparel_list = apparel_list)
