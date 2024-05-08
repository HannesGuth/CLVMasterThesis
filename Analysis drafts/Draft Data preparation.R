# Electronics

electronics = fread("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Datasets/Electronics retailer/durdata1_final.csv")
electronics = electronics[, c("HOUSEHOLD_ID", "TRANSACTION_NBR", "TRANSACTION_TOTAL", "TRANSACTION_DATE", "QUANTITY", "UNIT_PRICE", "EXTENDED_PRICE")]
electronics$Date = as.Date(electronics$TRANSACTION_DATE, format = "%d%b%Y")
electronics$Price = electronics$QUANTITY * electronics$UNIT_PRICE
elclv = electronics[, c("HOUSEHOLD_ID", "Date", "Price")]
colnames(elclv) = c("Id", "Date", "Price")
x = elclv[, min(Date), by = "Id"]
x = x[, .N, by = "V1"]
elclv = elclv[Date %in% "2001-01-06"]

clv.electronics <- clvdata(elclv,  
                           date.format="ymd", 
                           time.unit = "week",
                           estimation.split = 200,
                           name.id = "Id",
                           name.date = "Date",
                           name.price = "Price")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Gifts

gift = fread("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Master thesis/Datasets/Gift shop/gift.csv")
gift = gift[, c("ID", "date", "amount")]
colnames(gift) = c("Id", "Date", "Price")
x = gift[, min(Date), by = "Id"]
x = x[, .N, by = "V1"]
x[order(-x$N),]
x = gift[, min(Date), by = "Id"]
giftsmall = x[V1 %in% "2001-12-17"]
giftsmall = merge(x = gift, y = giftsmall, by = "Id")
giftsmall = giftsmall[, -4]
giftsmall$Date = as.Date(giftsmall$Date, format = "%d%b%Y")
